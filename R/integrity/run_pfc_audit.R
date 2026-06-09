suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
})

source(here::here("R", "integrity", "research_integrity.R"))

panel_path <- here::here("data", "processed", "play_for_contract_analysis_panel.csv")
retention_model_path <- here::here("output", "tables", "retention_overpay_model.csv")
out_dir <- here::here("output", "tables")

required_paths <- c(panel_path, retention_model_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing required input files:\n", paste0("- ", missing_paths, collapse = "\n"))
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

panel <- readr::read_csv(panel_path, show_col_types = FALSE)
published_model <- readr::read_csv(retention_model_path, show_col_types = FALSE)

required_cols <- c(
  "eligible_overpay", "retention_status", "overpay_residual", "tier",
  "trajectory", "signing_year"
)
missing_cols <- setdiff(required_cols, names(panel))
if (length(missing_cols) > 0) {
  stop("Panel is missing required columns:\n", paste0("- ", missing_cols, collapse = "\n"))
}

if (!("position" %in% names(panel)) && !("model_position_group" %in% names(panel))) {
  stop("Panel must include either position or model_position_group for position robustness cuts.")
}

retention_data <- panel %>%
  filter(
    .data$eligible_overpay,
    .data$retention_status %in% c("same_team", "new_team"),
    !is.na(.data$overpay_residual)
  ) %>%
  mutate(tier = dplyr::coalesce(.data$tier, "unknown"))

if (nrow(retention_data) != 1707L) {
  stop("Expected eligible same-team/new-team subset of n=1707. Found n=", nrow(retention_data), ".")
}

retention_counts <- retention_data %>% count(retention_status, name = "n")
if (!all(c("same_team", "new_team") %in% retention_counts$retention_status)) {
  stop("Retention subset must include both same_team and new_team rows.")
}

same_team_n <- retention_counts %>% filter(.data$retention_status == "same_team") %>% pull(.data$n)
new_team_n <- retention_counts %>% filter(.data$retention_status == "new_team") %>% pull(.data$n)
if (same_team_n != 1115L || new_team_n != 592L) {
  stop(
    "Expected published retention split of same_team=1115 and new_team=592. Found same_team=",
    same_team_n,
    ", new_team=",
    new_team_n,
    "."
  )
}

retention_model_data <- retention_data %>%
  filter(!is.na(.data$tier)) %>%
  mutate(
    retention_status = stats::relevel(factor(.data$retention_status), ref = "new_team"),
    tier = stats::relevel(factor(.data$tier), ref = "fringe")
  )

real_model <- stats::lm(overpay_residual ~ retention_status + tier, data = retention_model_data)
real_coef <- unname(stats::coef(real_model)[["retention_statussame_team"]])

published_coef <- published_model %>%
  filter(.data$term == "retention_statussame_team") %>%
  pull(.data$estimate)

if (length(published_coef) != 1L) {
  stop("Could not find unique published same-team coefficient in retention_overpay_model.csv.")
}

if (!isTRUE(all.equal(real_coef, published_coef[[1L]], tolerance = 1e-12))) {
  stop(
    "Reproduced same-team coefficient (",
    signif(real_coef, 12),
    ") does not match published coefficient (",
    signif(published_coef[[1L]], 12),
    "). Reconcile before proceeding."
  )
}

if ("model_position_group" %in% names(retention_model_data)) {
  retention_model_data <- retention_model_data %>%
    mutate(
      position_group = dplyr::case_when(
        .data$model_position_group == "forward" ~ "forwards",
        .data$model_position_group == "defense" ~ "defensemen",
        TRUE ~ NA_character_
      )
    )
} else {
  retention_model_data <- retention_model_data %>%
    mutate(
      position_group = dplyr::case_when(
        str_detect(.data$position, "LD|RD") & !str_detect(.data$position, "\\bC\\b|LW|RW") ~ "defensemen",
        str_detect(.data$position, "\\bC\\b|LW|RW") ~ "forwards",
        str_detect(.data$position, "LD|RD") ~ "defensemen",
        TRUE ~ NA_character_
      )
    )
}

year_counts <- retention_model_data %>%
  count(signing_year, name = "n") %>%
  arrange(.data$signing_year) %>%
  mutate(cum_n = cumsum(.data$n))

half_n <- sum(year_counts$n) / 2
era_split_year <- year_counts %>%
  filter(.data$cum_n >= half_n) %>%
  slice(1) %>%
  pull(.data$signing_year)

min_year <- min(retention_model_data$signing_year, na.rm = TRUE)
max_year <- max(retention_model_data$signing_year, na.rm = TRUE)
early_label <- paste0("early_", min_year, "_", era_split_year)
late_label <- paste0("late_", era_split_year + 1L, "_", max_year)

retention_model_data <- retention_model_data %>%
  mutate(
    era = dplyr::if_else(.data$signing_year <= era_split_year, early_label, late_label)
  )

falsification <- falsification_test(
  data = retention_model_data,
  outcome = "overpay_residual",
  treatment = "retention_status",
  controls = c("tier"),
  n_perms = 2000L,
  seed = 20260609L
) %>%
  mutate(
    model = "overpay_residual ~ retention_status + tier",
    treatment_term = "retention_statussame_team",
    sample_n = nrow(retention_model_data),
    seed = 20260609L
  ) %>%
  select(
    model,
    treatment_term,
    sample_n,
    seed,
    real_coefficient,
    null_mean,
    null_sd,
    n_perms,
    n_more_extreme,
    empirical_p_value
  )

subsample_specs <- list(
  list(col = "position_group", label = "position"),
  list(col = "trajectory", label = "trajectory"),
  list(col = "tier", label = "tier"),
  list(col = "era", label = "era")
)

subsample_results <- lapply(subsample_specs, function(spec) {
  subgroup_df <- retention_model_data %>%
    filter(!is.na(.data[[spec$col]]))

  subsample_robustness(
    data = subgroup_df,
    outcome = "overpay_residual",
    treatment = "retention_status",
    controls = c("tier"),
    subgroup_col = spec$col,
    min_n = 30L
  ) %>%
    mutate(
      subgroup_col = spec$label,
      full_sample_coefficient = real_coef
    )
})

subsample_robustness_table <- bind_rows(subsample_results) %>%
  mutate(
    subgroup_col = factor(.data$subgroup_col, levels = c("position", "trajectory", "tier", "era"))
  ) %>%
  arrange(.data$subgroup_col, .data$subgroup_value) %>%
  mutate(subgroup_col = as.character(.data$subgroup_col)) %>%
  select(
    subgroup_col,
    subgroup_value,
    n,
    coefficient,
    std_error,
    p_value,
    holds,
    full_sample_coefficient
  )

outlier_table <- outlier_sensitivity(
  data = retention_model_data,
  outcome = "overpay_residual",
  treatment = "retention_status",
  controls = c("tier"),
  trim_fractions = c(0, 0.01, 0.05)
) %>%
  mutate(
    model = "overpay_residual ~ retention_status + tier",
    treatment_term = "retention_statussame_team",
    full_sample_coefficient = real_coef
  ) %>%
  select(
    model,
    treatment_term,
    trim_fraction,
    n,
    coefficient,
    p_value,
    full_sample_coefficient
  )

readr::write_csv(falsification, file.path(out_dir, "audit_falsification.csv"))
readr::write_csv(subsample_robustness_table, file.path(out_dir, "audit_subsample_robustness.csv"))
readr::write_csv(outlier_table, file.path(out_dir, "audit_outlier_sensitivity.csv"))

line_break <- function() ""
fmt_num <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt_p <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  if (x < 0.0001) {
    return("<0.0001")
  }
  formatC(x, format = "f", digits = 4)
}

tier_holds <- subsample_robustness_table %>%
  filter(.data$subgroup_col == "tier")
trajectory_holds <- subsample_robustness_table %>%
  filter(.data$subgroup_col == "trajectory")
non_holding_rows <- subsample_robustness_table %>%
  filter(!.data$holds)

outlier_takeaway <- if (all(outlier_table$coefficient > 0, na.rm = TRUE) && all(outlier_table$p_value < 0.05, na.rm = TRUE)) {
  "The same-team coefficient stays positive and significant after 1 percent and 5 percent tail trimming."
} else {
  "At least one trimming level weakens or overturns the same-team effect."
}

summary_lines <- c(
  "# Audit Summary",
  line_break(),
  "## Falsification test",
  paste0("- Real same-team coefficient: ", fmt_num(falsification$real_coefficient[[1L]], 3), "."),
  paste0("- Null mean from shuffled treatment labels: ", fmt_num(falsification$null_mean[[1L]], 3), "."),
  paste0("- Null standard deviation: ", fmt_num(falsification$null_sd[[1L]], 3), "."),
  paste0("- Empirical p-value: ", fmt_p(falsification$empirical_p_value[[1L]]), " (", falsification$n_more_extreme[[1L]], " of ", falsification$n_perms[[1L]], " permutations as or more extreme)."),
  line_break(),
  "## Subsample robustness",
  paste0("- Position, trajectory, tier, and era subgroup models were run on the eligible same-team/new-team sample (n=", nrow(retention_model_data), ")."),
  paste0("- Era split used contract signing years ", early_label, " and ", late_label, "."),
  paste0("- Tier groups with holds=TRUE: ", sum(tier_holds$holds, na.rm = TRUE), " of ", nrow(tier_holds), "."),
  paste0("- Trajectory groups with holds=TRUE: ", sum(trajectory_holds$holds, na.rm = TRUE), " of ", nrow(trajectory_holds), "."),
  if (nrow(non_holding_rows) > 0) {
    paste0(
      "- Non-holding subgroups: ",
      paste0(non_holding_rows$subgroup_col, "=", non_holding_rows$subgroup_value, collapse = "; "),
      "."
    )
  } else {
    "- All reported subgroups meet the holds criterion."
  },
  "- The within-tier and within-trajectory cuts are the selection-relevant checks.",
  line_break(),
  "## Outlier sensitivity",
  paste0("- No-trim coefficient: ", fmt_num(outlier_table$coefficient[outlier_table$trim_fraction == 0][[1L]], 3), "."),
  paste0("- 1 percent trim coefficient: ", fmt_num(outlier_table$coefficient[outlier_table$trim_fraction == 0.01][[1L]], 3), "."),
  paste0("- 5 percent trim coefficient: ", fmt_num(outlier_table$coefficient[outlier_table$trim_fraction == 0.05][[1L]], 3), "."),
  paste0("- ", outlier_takeaway)
)

writeLines(summary_lines, con = file.path(out_dir, "audit_summary.md"))

message("Wrote audit outputs to ", out_dir)