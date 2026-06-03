# 03_overpay_metric.R
# Build cap-share-normalized expected-vs-actual TOI overpay metric.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
})

panel_path <- here::here("data", "processed", "player_contract_panel.csv")
performance_features_path <- here::here("data", "processed", "contract_performance_features.csv")
out_path <- here::here("data", "processed", "contract_overpay_features.csv")

required_paths <- c(panel_path, performance_features_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing required input files:\n", paste0("- ", missing_paths, collapse = "\n"))
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

position_group <- function(x) {
  x_up <- toupper(dplyr::coalesce(x, ""))
  dplyr::case_when(
    stringr::str_detect(x_up, "D") ~ "defense",
    stringr::str_detect(x_up, "C|LW|RW|W|F|L|R") ~ "forward",
    TRUE ~ NA_character_
  )
}

# NHL upper-limit cap ceiling by season (USD, millions).
# Flat-cap period retained explicitly for 2020-21 and 2021-22.
cap_table <- tibble::tribble(
  ~first_contract_season, ~cap_ceiling_millions,
  20122013L, 70.2,
  20132014L, 64.3,
  20142015L, 69.0,
  20152016L, 71.4,
  20162017L, 73.0,
  20172018L, 75.0,
  20182019L, 79.5,
  20192020L, 81.5,
  20202021L, 81.5,
  20212022L, 81.5,
  20222023L, 82.5,
  20232024L, 83.5,
  20242025L, 88.0,
  20252026L, 92.4
) %>%
  mutate(cap_ceiling = .data$cap_ceiling_millions * 1e6)

panel <- readr::read_csv(panel_path, show_col_types = FALSE)
performance_features <- readr::read_csv(performance_features_path, show_col_types = FALSE)

model_base <- panel %>%
  left_join(
    performance_features %>%
      select(
        contract_id,
        walk_year_position_group,
        post_signing_seasons_observed,
        post_signing_avg_toi_per_game,
        contract_complete
      ),
    by = "contract_id"
  ) %>%
  mutate(
    model_position_group = dplyr::coalesce(.data$walk_year_position_group, position_group(.data$position))
  ) %>%
  left_join(cap_table %>% select(first_contract_season, cap_ceiling), by = "first_contract_season") %>%
  mutate(
    aav_cap_share = dplyr::if_else(!is.na(.data$cap_ceiling) & .data$cap_ceiling > 0, .data$aav / .data$cap_ceiling, NA_real_),
    eligible_overpay =
      .data$retention_status != "entry" &
      !.data$is_extension &
      !.data$before_coverage_boundary &
      .data$post_signing_seasons_observed >= 1 &
      !is.na(.data$model_position_group) &
      !is.na(.data$aav_cap_share) &
      !is.na(.data$post_signing_avg_toi_per_game)
  )

eligible_model_data <- model_base %>%
  filter(.data$eligible_overpay)

if (nrow(eligible_model_data) < 20) {
  stop("Eligible overpay modeling sample is too small (<20 rows).")
}

if (length(unique(eligible_model_data$model_position_group)) < 2) {
  stop("Overpay model requires at least two position groups in eligible data.")
}

overpay_model <- stats::lm(
  post_signing_avg_toi_per_game ~ aav_cap_share * model_position_group,
  data = eligible_model_data
)

model_resid_sd <- stats::sigma(overpay_model)

overpay_features <- model_base %>%
  mutate(
    expected_toi = dplyr::if_else(
      .data$eligible_overpay,
      stats::predict(overpay_model, newdata = model_base),
      NA_real_
    ),
    overpay_residual = dplyr::if_else(
      .data$eligible_overpay,
      .data$post_signing_avg_toi_per_game - .data$expected_toi,
      NA_real_
    ),
    overpay_residual_std = dplyr::if_else(
      .data$eligible_overpay & !is.na(model_resid_sd) & model_resid_sd > 0,
      .data$overpay_residual / model_resid_sd,
      NA_real_
    ),
    partial_delivery_flag = dplyr::if_else(.data$eligible_overpay & !.data$contract_complete, TRUE, FALSE, missing = FALSE)
  ) %>%
  select(
    contract_id,
    model_position_group,
    cap_ceiling,
    aav_cap_share,
    eligible_overpay,
    expected_toi,
    overpay_residual,
    overpay_residual_std,
    partial_delivery_flag
  )

readr::write_csv(overpay_features, out_path)

resid_summary <- overpay_features %>%
  filter(.data$eligible_overpay, !is.na(.data$overpay_residual)) %>%
  summarise(
    n = n(),
    mean = mean(.data$overpay_residual),
    sd = stats::sd(.data$overpay_residual),
    min = min(.data$overpay_residual),
    max = max(.data$overpay_residual)
  )

extreme_overpays <- panel %>%
  select(contract_id, player_name, signing_year) %>%
  inner_join(overpay_features, by = "contract_id") %>%
  filter(.data$eligible_overpay) %>%
  arrange(.data$overpay_residual) %>%
  select(player_name, signing_year, model_position_group, overpay_residual) %>%
  head(10)

extreme_discounts <- panel %>%
  select(contract_id, player_name, signing_year) %>%
  inner_join(overpay_features, by = "contract_id") %>%
  filter(.data$eligible_overpay) %>%
  arrange(desc(.data$overpay_residual)) %>%
  select(player_name, signing_year, model_position_group, overpay_residual) %>%
  head(10)

message("Phase 4 - overpay metric summary")
message(sprintf("- contracts in panel: %s", nrow(panel)))
message(sprintf("- eligible_overpay: %s", sum(overpay_features$eligible_overpay, na.rm = TRUE)))
message(sprintf("- contracts flagged partial delivery: %s", sum(overpay_features$partial_delivery_flag, na.rm = TRUE)))
message("- model formula: post_signing_avg_toi_per_game ~ aav_cap_share * model_position_group")
message("- model fit summary:")
print(summary(overpay_model))
message("- residual distribution:")
print(resid_summary)
message("- largest negative residuals (overpays):")
print(extreme_overpays)
message("- largest positive residuals (discounts):")
print(extreme_discounts)
message(sprintf("- saved: %s", out_path))