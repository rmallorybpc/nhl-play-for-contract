suppressPackageStartupMessages({
  library(dplyr)
  library(rlang)
  library(tibble)
})

build_model_formula <- function(outcome, treatment, controls = character()) {
  if (!is.character(outcome) || length(outcome) != 1L || !nzchar(outcome)) {
    stop("outcome must be a single non-empty column name.")
  }
  if (!is.character(treatment) || length(treatment) != 1L || !nzchar(treatment)) {
    stop("treatment must be a single non-empty column name.")
  }

  terms <- c(treatment, controls)
  stats::reformulate(termlabels = terms, response = outcome)
}

is_nonconstant <- function(x) {
  x_non_na <- x[!is.na(x)]
  if (length(x_non_na) <= 1L) {
    return(FALSE)
  }
  length(unique(x_non_na)) > 1L
}

drop_constant_controls <- function(data, controls) {
  controls[vapply(controls, function(col) is_nonconstant(data[[col]]), logical(1L))]
}

get_treatment_term <- function(model, treatment) {
  coef_names <- names(stats::coef(model))
  matches <- coef_names[grepl(paste0("^", treatment), coef_names)]
  if (length(matches) < 1L) {
    stop("Treatment term not found in fitted model coefficients.")
  }
  matches[[1L]]
}

#' Run a permutation-based falsification test for a treatment effect.
#'
#' Fits outcome ~ treatment + controls on observed data, then repeatedly shuffles
#' the treatment assignment to build a null distribution of treatment coefficients.
#'
#' @param data Data frame containing outcome, treatment, and controls.
#' @param outcome Outcome column name.
#' @param treatment Treatment column name.
#' @param controls Character vector of control column names.
#' @param n_perms Number of permutations to run. Default 2000.
#' @param seed Integer seed for reproducibility. Default 123.
#'
#' @return A one-row tibble with real coefficient, null moments, and empirical p-value.
falsification_test <- function(
  data,
  outcome,
  treatment,
  controls = character(),
  n_perms = 2000L,
  seed = 123L
) {
  required_cols <- unique(c(outcome, treatment, controls))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  model_df <- data %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::filter(stats::complete.cases(.))

  if (nrow(model_df) < 3L) {
    stop("Insufficient complete rows to run falsification test.")
  }

  active_controls <- drop_constant_controls(model_df, controls)
  model_formula <- build_model_formula(outcome, treatment, active_controls)
  real_model <- stats::lm(model_formula, data = model_df)
  treatment_term <- get_treatment_term(real_model, treatment)
  real_coef <- unname(stats::coef(real_model)[[treatment_term]])

  set.seed(seed)
  permuted_coefs <- rep(NA_real_, n_perms)
  treatment_values <- model_df[[treatment]]

  for (i in seq_len(n_perms)) {
    shuffled_df <- model_df
    shuffled_df[[treatment]] <- sample(treatment_values, replace = FALSE)
    perm_model <- stats::lm(model_formula, data = shuffled_df)
    permuted_coefs[[i]] <- unname(stats::coef(perm_model)[[treatment_term]])
  }

  n_more_extreme <- sum(abs(permuted_coefs) >= abs(real_coef), na.rm = TRUE)
  empirical_p <- n_more_extreme / n_perms

  tibble::tibble(
    real_coefficient = real_coef,
    null_mean = mean(permuted_coefs, na.rm = TRUE),
    null_sd = stats::sd(permuted_coefs, na.rm = TRUE),
    n_perms = as.integer(n_perms),
    n_more_extreme = as.integer(n_more_extreme),
    empirical_p_value = empirical_p
  )
}

#' Re-estimate a treatment model within each subgroup level.
#'
#' For each subgroup level with at least min_n observations, fits
#' outcome ~ treatment + controls after dropping controls that are constant
#' within that subgroup.
#'
#' @param data Data frame containing outcome, treatment, controls, and subgroup.
#' @param outcome Outcome column name.
#' @param treatment Treatment column name.
#' @param controls Character vector of control column names.
#' @param subgroup_col Subgroup column name.
#' @param min_n Minimum subgroup sample size required for estimation. Default 30.
#'
#' @return A tibble with one row per subgroup level and coefficient diagnostics.
subsample_robustness <- function(
  data,
  outcome,
  treatment,
  controls = character(),
  subgroup_col,
  min_n = 30L
) {
  required_cols <- unique(c(outcome, treatment, controls, subgroup_col))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  full_df <- data %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::filter(stats::complete.cases(.))

  if (nrow(full_df) < as.integer(min_n)) {
    stop("Insufficient complete rows to run subsample robustness.")
  }

  full_controls <- drop_constant_controls(full_df, controls)
  full_formula <- build_model_formula(outcome, treatment, full_controls)
  full_model <- stats::lm(full_formula, data = full_df)
  treatment_term <- get_treatment_term(full_model, treatment)
  full_sign <- sign(unname(stats::coef(full_model)[[treatment_term]]))

  level_values <- sort(unique(full_df[[subgroup_col]]))

  rows <- lapply(level_values, function(level_val) {
    level_df <- full_df %>% dplyr::filter(.data[[subgroup_col]] == level_val)
    n_level <- nrow(level_df)
    if (n_level < as.integer(min_n)) {
      return(NULL)
    }

    if (!is_nonconstant(level_df[[treatment]])) {
      return(NULL)
    }

    active_controls <- drop_constant_controls(level_df, controls)
    level_formula <- build_model_formula(outcome, treatment, active_controls)
    level_model <- stats::lm(level_formula, data = level_df)

    coef_table <- summary(level_model)$coefficients
    if (!(treatment_term %in% rownames(coef_table))) {
      return(NULL)
    }

    coefficient <- unname(coef_table[treatment_term, "Estimate"])
    std_error <- unname(coef_table[treatment_term, "Std. Error"])
    p_value <- unname(coef_table[treatment_term, "Pr(>|t|)"])

    tibble::tibble(
      subgroup_col = subgroup_col,
      subgroup_value = as.character(level_val),
      n = n_level,
      coefficient = coefficient,
      std_error = std_error,
      p_value = p_value,
      holds = (sign(coefficient) == full_sign) & is.finite(p_value) & p_value < 0.05
    )
  })

  dplyr::bind_rows(rows)
}

#' Estimate treatment-effect stability after trimming outcome tails.
#'
#' For each trim fraction, removes that fraction from each tail of the outcome
#' distribution, then refits outcome ~ treatment + controls.
#'
#' @param data Data frame containing outcome, treatment, and controls.
#' @param outcome Outcome column name.
#' @param treatment Treatment column name.
#' @param controls Character vector of control column names.
#' @param trim_fractions Numeric vector of two-sided trimming fractions.
#'
#' @return A tibble with one row per trim level and model estimates.
outlier_sensitivity <- function(
  data,
  outcome,
  treatment,
  controls = character(),
  trim_fractions = c(0, 0.01, 0.05)
) {
  required_cols <- unique(c(outcome, treatment, controls))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  model_df <- data %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::filter(stats::complete.cases(.))

  if (nrow(model_df) < 3L) {
    stop("Insufficient complete rows to run outlier sensitivity.")
  }

  active_controls <- drop_constant_controls(model_df, controls)
  model_formula <- build_model_formula(outcome, treatment, active_controls)
  base_model <- stats::lm(model_formula, data = model_df)
  treatment_term <- get_treatment_term(base_model, treatment)

  results <- lapply(trim_fractions, function(trim_fraction) {
    if (!is.numeric(trim_fraction) || length(trim_fraction) != 1L || trim_fraction < 0 || trim_fraction >= 0.5) {
      stop("Each trim fraction must be a single numeric value in [0, 0.5).")
    }

    outcome_vals <- model_df[[outcome]]
    lower <- stats::quantile(outcome_vals, probs = trim_fraction, names = FALSE)
    upper <- stats::quantile(outcome_vals, probs = 1 - trim_fraction, names = FALSE)
    trimmed_df <- model_df %>% dplyr::filter(.data[[outcome]] >= lower, .data[[outcome]] <= upper)

    trimmed_controls <- drop_constant_controls(trimmed_df, controls)
    if (!is_nonconstant(trimmed_df[[treatment]])) {
      return(tibble::tibble(
        trim_fraction = as.numeric(trim_fraction),
        n = nrow(trimmed_df),
        coefficient = NA_real_,
        p_value = NA_real_
      ))
    }

    trim_formula <- build_model_formula(outcome, treatment, trimmed_controls)
    trim_model <- stats::lm(trim_formula, data = trimmed_df)
    coef_table <- summary(trim_model)$coefficients

    if (!(treatment_term %in% rownames(coef_table))) {
      return(tibble::tibble(
        trim_fraction = as.numeric(trim_fraction),
        n = nrow(trimmed_df),
        coefficient = NA_real_,
        p_value = NA_real_
      ))
    }

    tibble::tibble(
      trim_fraction = as.numeric(trim_fraction),
      n = nrow(trimmed_df),
      coefficient = unname(coef_table[treatment_term, "Estimate"]),
      p_value = unname(coef_table[treatment_term, "Pr(>|t|)"])
    )
  })

  dplyr::bind_rows(results)
}