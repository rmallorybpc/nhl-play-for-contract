# 01_phase5_analysis.R
# Run Phase 5 analysis from the analysis panel and write tables/findings.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(tidyr)
  library(stringr)
})

panel_path <- here::here("data", "processed", "play_for_contract_analysis_panel.csv")
skaters_path <- here::here("data", "processed", "nhlscraper_skaters_clean.csv")
out_dir <- here::here("output", "tables")

required_paths <- c(panel_path, skaters_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing required input files:\n", paste0("- ", missing_paths, collapse = "\n"))
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

season_step <- 10001L
trajectory_threshold_minutes <- 0.35

season_offset <- function(season, n) {
  season - as.integer(n) * season_step
}

season_sequence <- function(first_season, n_years) {
  if (is.na(first_season) || is.na(n_years) || n_years <= 0) {
    return(integer(0))
  }
  first_season + (seq_len(as.integer(n_years)) - 1L) * season_step
}

safe_quantile <- function(x, p) {
  vals <- x[!is.na(x)]
  if (length(vals) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(vals, probs = p, names = FALSE))
}

safe_sd <- function(x) {
  vals <- x[!is.na(x)]
  if (length(vals) < 2) {
    return(NA_real_)
  }
  stats::sd(vals)
}

format_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

format_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

panel <- readr::read_csv(panel_path, show_col_types = FALSE)
skaters_raw <- readr::read_csv(skaters_path, show_col_types = FALSE)

required_panel_cols <- c(
  "contract_id", "player_id", "player_name", "signing_year", "walk_year_season",
  "first_contract_season", "contract_years", "age_at_signing", "retention_status",
  "signing_team", "previous_team",
  "captaincy_status", "tier", "trajectory", "eligible_walk_year", "eligible_overpay",
  "walk_year_toi_per_game", "post_signing_toi_change", "post_signing_points_change",
  "aav_cap_share", "overpay_residual"
)

missing_panel_cols <- setdiff(required_panel_cols, names(panel))
if (length(missing_panel_cols) > 0) {
  stop("Analysis panel is missing required columns:\n", paste0("- ", missing_panel_cols, collapse = "\n"))
}

if (!all(c("player_id", "season") %in% names(skaters_raw))) {
  stop("Skaters file must include player_id and season columns.")
}

skaters <- skaters_raw %>%
  filter(!is.na(.data$player_id), !is.na(.data$season)) %>%
  mutate(
    games_played = dplyr::coalesce(.data$games_played, 0),
    points = dplyr::coalesce(.data$points, 0),
    time_on_ice_total_minutes = dplyr::coalesce(.data$time_on_ice_total_minutes, 0)
  ) %>%
  group_by(.data$player_id, .data$season) %>%
  summarise(
    games_played = sum(.data$games_played, na.rm = TRUE),
    points = sum(.data$points, na.rm = TRUE),
    time_on_ice_total_minutes = sum(.data$time_on_ice_total_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    toi_per_game = dplyr::if_else(.data$games_played > 0, .data$time_on_ice_total_minutes / .data$games_played, NA_real_),
    points_per_game = dplyr::if_else(.data$games_played > 0, .data$points / .data$games_played, NA_real_)
  )

# Part 0: Coverage and sample transparency.
contracts_by_signing_year <- panel %>%
  count(signing_year, name = "contract_n") %>%
  arrange(signing_year)

eligible_sample_sizes <- tibble::tibble(
  sample = c("eligible_walk_year", "eligible_overpay"),
  n = c(
    sum(panel$eligible_walk_year, na.rm = TRUE),
    sum(panel$eligible_overpay, na.rm = TRUE)
  )
)

tier_trajectory_bucket_sizes <- panel %>%
  filter(.data$eligible_walk_year) %>%
  mutate(
    tier = dplyr::coalesce(.data$tier, "unknown"),
    trajectory = dplyr::coalesce(.data$trajectory, "unknown")
  ) %>%
  count(tier, trajectory, name = "n") %>%
  arrange(desc(.data$n), .data$tier, .data$trajectory)

retention_distribution <- panel %>%
  mutate(retention_status = dplyr::coalesce(.data$retention_status, "unknown")) %>%
  count(retention_status, name = "n") %>%
  arrange(desc(.data$n), .data$retention_status)

captain_contract_count <- tibble::tibble(
  captain_contract_count = sum(panel$captaincy_status == "C", na.rm = TRUE),
  captain_contract_count_eligible_overpay = sum(panel$captaincy_status == "C" & panel$eligible_overpay, na.rm = TRUE)
)

readr::write_csv(contracts_by_signing_year, file.path(out_dir, "coverage_contract_count_by_signing_year.csv"))
readr::write_csv(eligible_sample_sizes, file.path(out_dir, "coverage_eligible_sample_sizes.csv"))
readr::write_csv(tier_trajectory_bucket_sizes, file.path(out_dir, "coverage_tier_trajectory_bucket_sizes.csv"))
readr::write_csv(retention_distribution, file.path(out_dir, "coverage_retention_distribution.csv"))
readr::write_csv(captain_contract_count, file.path(out_dir, "coverage_captain_contract_count.csv"))

# Part 1: Walk-year effect segmented by tier x trajectory.
walk_year_data <- panel %>%
  filter(.data$eligible_walk_year) %>%
  mutate(
    tier = dplyr::coalesce(.data$tier, "unknown"),
    trajectory = dplyr::coalesce(.data$trajectory, "unknown")
  )

walk_post_signing_by_bucket <- walk_year_data %>%
  group_by(.data$tier, .data$trajectory) %>%
  summarise(
    n = n(),
    mean_post_signing_toi_change = mean(.data$post_signing_toi_change, na.rm = TRUE),
    median_post_signing_toi_change = stats::median(.data$post_signing_toi_change, na.rm = TRUE),
    sd_post_signing_toi_change = safe_sd(.data$post_signing_toi_change),
    iqr_post_signing_toi_change = stats::IQR(.data$post_signing_toi_change, na.rm = TRUE),
    mean_post_signing_points_change = mean(.data$post_signing_points_change, na.rm = TRUE),
    median_post_signing_points_change = stats::median(.data$post_signing_points_change, na.rm = TRUE),
    sd_post_signing_points_change = safe_sd(.data$post_signing_points_change),
    iqr_post_signing_points_change = stats::IQR(.data$post_signing_points_change, na.rm = TRUE),
    .groups = "drop"
  )

walk_trend_lookup <- walk_year_data %>%
  select(contract_id, player_id, walk_year_season, walk_year_toi_per_game) %>%
  rowwise() %>%
  mutate(
    prior_seasons = list(c(
      season_offset(.data$walk_year_season, 1L),
      season_offset(.data$walk_year_season, 2L),
      season_offset(.data$walk_year_season, 3L)
    ))
  ) %>%
  ungroup() %>%
  tidyr::unnest_longer(prior_seasons, values_to = "season") %>%
  left_join(
    skaters %>%
      select(player_id, season, toi_per_game),
    by = c("player_id", "season")
  ) %>%
  filter(!is.na(.data$toi_per_game)) %>%
  group_by(.data$contract_id, .data$walk_year_season, .data$walk_year_toi_per_game) %>%
  summarise(
    prior_toi_seasons_found = n(),
    expected_walk_year_toi_from_trend = if (n() >= 2) {
      mod <- stats::lm(toi_per_game ~ season, data = dplyr::pick(season, toi_per_game))
      as.numeric(stats::predict(mod, newdata = data.frame(season = dplyr::first(.data$walk_year_season))))
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    walk_year_trend_delta = .data$walk_year_toi_per_game - .data$expected_walk_year_toi_from_trend,
    walk_year_boost_class = dplyr::case_when(
      is.na(.data$walk_year_trend_delta) ~ "insufficient_history",
      .data$walk_year_trend_delta >= trajectory_threshold_minutes ~ "spike_above_trend",
      .data$walk_year_trend_delta <= -trajectory_threshold_minutes ~ "below_trend",
      TRUE ~ "on_trend"
    )
  )

walk_year_data <- walk_year_data %>%
  left_join(
    walk_trend_lookup %>%
      select(contract_id, prior_toi_seasons_found, expected_walk_year_toi_from_trend, walk_year_trend_delta, walk_year_boost_class),
    by = "contract_id"
  )

walk_boost_by_bucket <- walk_year_data %>%
  group_by(.data$tier, .data$trajectory) %>%
  summarise(
    n_with_trend = sum(!is.na(.data$walk_year_trend_delta)),
    mean_walk_year_trend_delta = mean(.data$walk_year_trend_delta, na.rm = TRUE),
    median_walk_year_trend_delta = stats::median(.data$walk_year_trend_delta, na.rm = TRUE),
    sd_walk_year_trend_delta = safe_sd(.data$walk_year_trend_delta),
    share_spike_above_trend = mean(.data$walk_year_boost_class == "spike_above_trend", na.rm = TRUE),
    share_on_trend = mean(.data$walk_year_boost_class == "on_trend", na.rm = TRUE),
    share_below_trend = mean(.data$walk_year_boost_class == "below_trend", na.rm = TRUE),
    .groups = "drop"
  )

walk_year_effect_by_bucket <- walk_post_signing_by_bucket %>%
  full_join(walk_boost_by_bucket, by = c("tier", "trajectory")) %>%
  arrange(desc(.data$n), .data$tier, .data$trajectory)

walk_year_effect_overall <- walk_year_data %>%
  summarise(
    tier = "ALL",
    trajectory = "ALL",
    n = n(),
    mean_post_signing_toi_change = mean(.data$post_signing_toi_change, na.rm = TRUE),
    median_post_signing_toi_change = stats::median(.data$post_signing_toi_change, na.rm = TRUE),
    sd_post_signing_toi_change = safe_sd(.data$post_signing_toi_change),
    iqr_post_signing_toi_change = stats::IQR(.data$post_signing_toi_change, na.rm = TRUE),
    mean_post_signing_points_change = mean(.data$post_signing_points_change, na.rm = TRUE),
    median_post_signing_points_change = stats::median(.data$post_signing_points_change, na.rm = TRUE),
    sd_post_signing_points_change = safe_sd(.data$post_signing_points_change),
    iqr_post_signing_points_change = stats::IQR(.data$post_signing_points_change, na.rm = TRUE),
    n_with_trend = sum(!is.na(.data$walk_year_trend_delta)),
    mean_walk_year_trend_delta = mean(.data$walk_year_trend_delta, na.rm = TRUE),
    median_walk_year_trend_delta = stats::median(.data$walk_year_trend_delta, na.rm = TRUE),
    sd_walk_year_trend_delta = safe_sd(.data$walk_year_trend_delta),
    share_spike_above_trend = mean(.data$walk_year_boost_class == "spike_above_trend", na.rm = TRUE),
    share_on_trend = mean(.data$walk_year_boost_class == "on_trend", na.rm = TRUE),
    share_below_trend = mean(.data$walk_year_boost_class == "below_trend", na.rm = TRUE)
  )

walk_year_effect_by_bucket <- bind_rows(walk_year_effect_by_bucket, walk_year_effect_overall)
readr::write_csv(walk_year_effect_by_bucket, file.path(out_dir, "walk_year_effect_by_bucket.csv"))

walk_model_data <- walk_year_data %>%
  filter(
    !is.na(.data$post_signing_toi_change),
    !is.na(.data$tier),
    !is.na(.data$trajectory),
    !is.na(.data$age_at_signing)
  ) %>%
  mutate(
    tier = stats::relevel(factor(.data$tier), ref = "fringe"),
    trajectory = stats::relevel(factor(.data$trajectory), ref = "insufficient_history")
  )

walk_model <- NULL
walk_model_coefs <- tibble::tibble()
if (nrow(walk_model_data) >= 60 && dplyr::n_distinct(walk_model_data$tier) >= 2 && dplyr::n_distinct(walk_model_data$trajectory) >= 2) {
  walk_model <- stats::lm(post_signing_toi_change ~ tier + trajectory + age_at_signing, data = walk_model_data)
  walk_coef_mat <- summary(walk_model)$coefficients
  walk_model_coefs <- tibble::as_tibble(walk_coef_mat, rownames = "term") %>%
    rename(
      estimate = "Estimate",
      std_error = "Std. Error",
      t_value = "t value",
      p_value = "Pr(>|t|)"
    ) %>%
    mutate(model = "post_signing_toi_change ~ tier + trajectory + age_at_signing")
}

readr::write_csv(walk_model_coefs, file.path(out_dir, "walk_year_toi_change_model.csv"))

# Part 2: Retention-overpay comparison.
retention_data <- panel %>%
  filter(
    .data$eligible_overpay,
    .data$retention_status %in% c("same_team", "new_team"),
    !is.na(.data$overpay_residual)
  ) %>%
  mutate(tier = dplyr::coalesce(.data$tier, "unknown"))

retention_overpay_comparison <- retention_data %>%
  group_by(.data$retention_status) %>%
  summarise(
    n = n(),
    mean_overpay_residual = mean(.data$overpay_residual, na.rm = TRUE),
    median_overpay_residual = stats::median(.data$overpay_residual, na.rm = TRUE),
    sd_overpay_residual = safe_sd(.data$overpay_residual),
    iqr_overpay_residual = stats::IQR(.data$overpay_residual, na.rm = TRUE),
    p10_overpay_residual = safe_quantile(.data$overpay_residual, 0.10),
    p90_overpay_residual = safe_quantile(.data$overpay_residual, 0.90),
    .groups = "drop"
  )

same_team_mean <- retention_overpay_comparison %>%
  filter(.data$retention_status == "same_team") %>%
  pull(.data$mean_overpay_residual)

new_team_mean <- retention_overpay_comparison %>%
  filter(.data$retention_status == "new_team") %>%
  pull(.data$mean_overpay_residual)

retention_overpay_comparison <- bind_rows(
  retention_overpay_comparison,
  tibble::tibble(
    retention_status = "difference_same_minus_new",
    n = nrow(retention_data),
    mean_overpay_residual = same_team_mean - new_team_mean,
    median_overpay_residual = NA_real_,
    sd_overpay_residual = NA_real_,
    iqr_overpay_residual = NA_real_,
    p10_overpay_residual = NA_real_,
    p90_overpay_residual = NA_real_
  )
)

readr::write_csv(retention_overpay_comparison, file.path(out_dir, "retention_overpay_comparison.csv"))

retention_model_data <- retention_data %>%
  filter(!is.na(.data$tier)) %>%
  mutate(
    retention_status = stats::relevel(factor(.data$retention_status), ref = "new_team"),
    tier = stats::relevel(factor(.data$tier), ref = "fringe")
  )

retention_model <- NULL
retention_model_coefs <- tibble::as_tibble(matrix(ncol = 0, nrow = 0))
if (nrow(retention_model_data) >= 60 && dplyr::n_distinct(retention_model_data$retention_status) == 2) {
  retention_model <- stats::lm(overpay_residual ~ retention_status + tier, data = retention_model_data)
  retention_coef_mat <- summary(retention_model)$coefficients
  retention_model_coefs <- tibble::as_tibble(retention_coef_mat, rownames = "term") %>%
    rename(
      estimate = "Estimate",
      std_error = "Std. Error",
      t_value = "t value",
      p_value = "Pr(>|t|)"
    ) %>%
    mutate(model = "overpay_residual ~ retention_status + tier")
}

readr::write_csv(retention_model_coefs, file.path(out_dir, "retention_overpay_model.csv"))

# Part 3: Discount profile (descriptive).
discount_data <- panel %>%
  filter(.data$eligible_overpay, !is.na(.data$overpay_residual), .data$overpay_residual > 0) %>%
  mutate(
    tier = dplyr::coalesce(.data$tier, "unknown"),
    trajectory = dplyr::coalesce(.data$trajectory, "unknown"),
    retention_status = dplyr::coalesce(.data$retention_status, "unknown")
  )

discount_n <- nrow(discount_data)

discount_profile <- bind_rows(
  discount_data %>%
    count(level = .data$tier, name = "n") %>%
    mutate(profile_dimension = "tier", share = .data$n / discount_n),
  discount_data %>%
    count(level = .data$trajectory, name = "n") %>%
    mutate(profile_dimension = "trajectory", share = .data$n / discount_n),
  discount_data %>%
    count(level = .data$retention_status, name = "n") %>%
    mutate(profile_dimension = "retention_status", share = .data$n / discount_n),
  tibble::tibble(
    profile_dimension = "age_distribution",
    level = "overall",
    n = discount_n,
    share = 1,
    age_mean = mean(discount_data$age_at_signing, na.rm = TRUE),
    age_median = stats::median(discount_data$age_at_signing, na.rm = TRUE),
    age_p25 = safe_quantile(discount_data$age_at_signing, 0.25),
    age_p75 = safe_quantile(discount_data$age_at_signing, 0.75)
  )
) %>%
  mutate(
    age_mean = dplyr::if_else(.data$profile_dimension == "age_distribution", .data$age_mean, NA_real_),
    age_median = dplyr::if_else(.data$profile_dimension == "age_distribution", .data$age_median, NA_real_),
    age_p25 = dplyr::if_else(.data$profile_dimension == "age_distribution", .data$age_p25, NA_real_),
    age_p75 = dplyr::if_else(.data$profile_dimension == "age_distribution", .data$age_p75, NA_real_)
  ) %>%
  arrange(.data$profile_dimension, desc(.data$n), .data$level)

readr::write_csv(discount_profile, file.path(out_dir, "discount_profile.csv"))

# Part 4: Captaincy lens (descriptive).
captaincy_data <- panel %>%
  filter(.data$eligible_overpay, !is.na(.data$overpay_residual)) %>%
  mutate(captain_group = dplyr::if_else(.data$captaincy_status == "C", "captain", "non_captain"))

captaincy_summary <- captaincy_data %>%
  group_by(.data$captain_group) %>%
  summarise(
    n = n(),
    mean_overpay_residual = mean(.data$overpay_residual, na.rm = TRUE),
    median_overpay_residual = stats::median(.data$overpay_residual, na.rm = TRUE),
    sd_overpay_residual = safe_sd(.data$overpay_residual),
    iqr_overpay_residual = stats::IQR(.data$overpay_residual, na.rm = TRUE),
    .groups = "drop"
  )

captain_contracts <- captaincy_data %>%
  filter(.data$captain_group == "captain") %>%
  arrange(desc(.data$overpay_residual)) %>%
  select(player_name, signing_year, retention_status, tier, trajectory, overpay_residual)

captaincy_lens <- bind_rows(
  captaincy_summary %>%
    mutate(
      row_type = "summary",
      player_name = NA_character_,
      signing_year = NA_integer_,
      retention_status = NA_character_,
      tier = NA_character_,
      trajectory = NA_character_,
      overpay_residual = NA_real_
    ) %>%
    select(row_type, captain_group, n, mean_overpay_residual, median_overpay_residual, sd_overpay_residual, iqr_overpay_residual, player_name, signing_year, retention_status, tier, trajectory, overpay_residual),
  captain_contracts %>%
    mutate(
      row_type = "captain_contract",
      captain_group = "captain",
      n = NA_integer_,
      mean_overpay_residual = NA_real_,
      median_overpay_residual = NA_real_,
      sd_overpay_residual = NA_real_,
      iqr_overpay_residual = NA_real_
    ) %>%
    select(row_type, captain_group, n, mean_overpay_residual, median_overpay_residual, sd_overpay_residual, iqr_overpay_residual, player_name, signing_year, retention_status, tier, trajectory, overpay_residual)
)

readr::write_csv(captaincy_lens, file.path(out_dir, "captaincy_lens.csv"))

# Part 5: Overpay and discount extremes (raw vs material).
usage_by_contract <- panel %>%
  select(contract_id, player_id, first_contract_season, contract_years) %>%
  rowwise() %>%
  mutate(post_seasons = list(season_sequence(.data$first_contract_season, .data$contract_years))) %>%
  ungroup() %>%
  tidyr::unnest_longer(post_seasons, values_to = "season") %>%
  left_join(
    skaters %>%
      select(player_id, season, games_played, time_on_ice_total_minutes),
    by = c("player_id", "season")
  ) %>%
  group_by(.data$contract_id) %>%
  summarise(
    post_signing_games_played = sum(dplyr::coalesce(.data$games_played, 0), na.rm = TRUE),
    post_signing_toi_total_minutes = sum(dplyr::coalesce(.data$time_on_ice_total_minutes, 0), na.rm = TRUE),
    .groups = "drop"
  )

if ("partial_delivery_flag" %in% names(panel)) {
  panel_partial_delivery_flag <- dplyr::coalesce(as.logical(panel$partial_delivery_flag), FALSE)
} else if ("contract_complete" %in% names(panel)) {
  panel_partial_delivery_flag <- !dplyr::coalesce(as.logical(panel$contract_complete), TRUE)
} else {
  panel_partial_delivery_flag <- rep(FALSE, nrow(panel))
}

panel_with_partial_flag <- panel %>%
  mutate(partial_delivery_flag = panel_partial_delivery_flag)

extreme_base <- panel_with_partial_flag %>%
  filter(.data$eligible_overpay, !is.na(.data$overpay_residual)) %>%
  left_join(usage_by_contract, by = "contract_id") %>%
  mutate(
    partial_delivery_flag = dplyr::coalesce(.data$partial_delivery_flag, FALSE),
    post_signing_games_played = dplyr::coalesce(.data$post_signing_games_played, 0),
    post_signing_toi_total_minutes = dplyr::coalesce(.data$post_signing_toi_total_minutes, 0)
  )

top_n_extremes <- 25L

raw_overpays <- extreme_base %>%
  arrange(.data$overpay_residual) %>%
  slice_head(n = top_n_extremes) %>%
  mutate(extreme_type = "overpay", rank = dplyr::row_number())

raw_discounts <- extreme_base %>%
  arrange(desc(.data$overpay_residual)) %>%
  slice_head(n = top_n_extremes) %>%
  mutate(extreme_type = "discount", rank = dplyr::row_number())

overpay_extremes_raw <- bind_rows(raw_overpays, raw_discounts) %>%
  select(
    extreme_type,
    rank,
    player_name,
    signing_year,
    retention_status,
    tier,
    trajectory,
    captaincy_status,
    aav_cap_share,
    post_signing_games_played,
    post_signing_toi_total_minutes,
    partial_delivery_flag,
    overpay_residual
  )

readr::write_csv(overpay_extremes_raw, file.path(out_dir, "overpay_extremes_raw.csv"))

material_cap_share_floor <- 0.03
material_games_floor <- 82
material_toi_floor <- 1000

material_base <- extreme_base %>%
  mutate(
    passes_material_contract_lens =
      !is.na(.data$aav_cap_share) &
      .data$aav_cap_share >= material_cap_share_floor &
      (.data$post_signing_games_played >= material_games_floor | .data$post_signing_toi_total_minutes >= material_toi_floor)
  ) %>%
  filter(.data$passes_material_contract_lens)

material_overpays <- material_base %>%
  arrange(.data$overpay_residual) %>%
  slice_head(n = top_n_extremes) %>%
  mutate(extreme_type = "overpay", rank = dplyr::row_number())

material_discounts <- material_base %>%
  arrange(desc(.data$overpay_residual)) %>%
  slice_head(n = top_n_extremes) %>%
  mutate(extreme_type = "discount", rank = dplyr::row_number())

overpay_extremes_material <- bind_rows(material_overpays, material_discounts) %>%
  mutate(
    previous_team = dplyr::na_if(.data$previous_team, "NA"),
    signing_team = dplyr::na_if(.data$signing_team, "NA"),
    material_cap_share_floor = material_cap_share_floor,
    material_games_floor = material_games_floor,
    material_toi_floor = material_toi_floor
  ) %>%
  select(
    extreme_type,
    rank,
    player_name,
    signing_year,
    previous_team,
    signing_team,
    retention_status,
    tier,
    trajectory,
    captaincy_status,
    aav_cap_share,
    post_signing_games_played,
    post_signing_toi_total_minutes,
    partial_delivery_flag,
    overpay_residual,
    material_cap_share_floor,
    material_games_floor,
    material_toi_floor
  )

readr::write_csv(overpay_extremes_material, file.path(out_dir, "overpay_extremes_material.csv"))

# Part 6: Markdown findings summary with concrete numbers.
aggregate_walk <- walk_year_effect_by_bucket %>% filter(.data$tier == "ALL", .data$trajectory == "ALL")

bucket_effect_focus <- walk_year_effect_by_bucket %>%
  filter(.data$tier != "ALL", .data$trajectory != "ALL", .data$n >= 20) %>%
  arrange(.data$mean_post_signing_toi_change)

bucket_drop_examples <- bucket_effect_focus %>%
  slice_head(n = 3) %>%
  mutate(
    text = paste0(
      .data$tier, " x ", .data$trajectory,
      " (n=", .data$n, ")",
      ": post-signing TOI change ", format_num(.data$mean_post_signing_toi_change),
      ", walk-year trend delta ", format_num(.data$mean_walk_year_trend_delta)
    )
  ) %>%
  pull(.data$text)

bucket_hold_examples <- bucket_effect_focus %>%
  filter(.data$mean_post_signing_toi_change >= 0) %>%
  arrange(desc(.data$mean_post_signing_toi_change)) %>%
  slice_head(n = 3) %>%
  mutate(
    text = paste0(
      .data$tier, " x ", .data$trajectory,
      " (n=", .data$n, ")",
      ": post-signing TOI change ", format_num(.data$mean_post_signing_toi_change),
      ", walk-year trend delta ", format_num(.data$mean_walk_year_trend_delta)
    )
  ) %>%
  pull(.data$text)

ret_same <- retention_overpay_comparison %>%
  filter(.data$retention_status == "same_team")
ret_new <- retention_overpay_comparison %>%
  filter(.data$retention_status == "new_team")
ret_diff <- retention_overpay_comparison %>%
  filter(.data$retention_status == "difference_same_minus_new")

ret_same_coef <- retention_model_coefs %>%
  filter(.data$term == "retention_statussame_team")

walk_age_coef <- walk_model_coefs %>%
  filter(.data$term == "age_at_signing")

capt_summary_captain <- captaincy_summary %>%
  filter(.data$captain_group == "captain")
capt_summary_non <- captaincy_summary %>%
  filter(.data$captain_group == "non_captain")

captain_names <- captain_contracts %>%
  mutate(text = paste0(.data$player_name, " (", .data$signing_year, ": ", format_num(.data$overpay_residual), ")")) %>%
  pull(.data$text)

discount_top_tier <- discount_profile %>%
  filter(.data$profile_dimension == "tier") %>%
  arrange(desc(.data$n)) %>%
  slice_head(n = 1)

discount_top_traj <- discount_profile %>%
  filter(.data$profile_dimension == "trajectory") %>%
  arrange(desc(.data$n)) %>%
  slice_head(n = 1)

discount_top_retention <- discount_profile %>%
  filter(.data$profile_dimension == "retention_status") %>%
  arrange(desc(.data$n)) %>%
  slice_head(n = 1)

discount_age <- discount_profile %>%
  filter(.data$profile_dimension == "age_distribution") %>%
  slice_head(n = 1)

bergeron_in_data <- panel %>%
  filter(str_detect(.data$player_name, regex("Bergeron", ignore_case = TRUE))) %>%
  nrow() > 0

bergeron_in_discounts <- discount_data %>%
  filter(str_detect(.data$player_name, regex("Bergeron", ignore_case = TRUE))) %>%
  nrow() > 0

material_discount_examples <- overpay_extremes_material %>%
  filter(.data$extreme_type == "discount") %>%
  arrange(.data$rank) %>%
  slice_head(n = 5) %>%
  mutate(text = paste0(.data$player_name, " (", .data$signing_year, ": ", format_num(.data$overpay_residual), ")")) %>%
  pull(.data$text)

material_overpay_examples <- overpay_extremes_material %>%
  filter(.data$extreme_type == "overpay") %>%
  arrange(.data$rank) %>%
  slice_head(n = 5) %>%
  mutate(text = paste0(.data$player_name, " (", .data$signing_year, ": ", format_num(.data$overpay_residual), ")")) %>%
  pull(.data$text)

signing_year_distribution_text <- contracts_by_signing_year %>%
  mutate(text = paste0(.data$signing_year, "=", .data$contract_n)) %>%
  pull(.data$text) %>%
  paste(collapse = ", ")

thin_bucket_labels <- tier_trajectory_bucket_sizes %>%
  filter(.data$n < 20) %>%
  mutate(label = paste0(.data$tier, " x ", .data$trajectory, " (", .data$n, ")")) %>%
  pull(.data$label)

findings_lines <- c(
  "# Phase 5 Findings: Play-for-Contract Analysis",
  "",
  "## Part 0 - Coverage and sample transparency",
  paste0("- Contracts in panel: ", nrow(panel), "."),
  paste0("- Contract count by signing year: ", signing_year_distribution_text, "."),
  paste0("- Eligible walk-year sample: ", sum(panel$eligible_walk_year, na.rm = TRUE), "."),
  paste0("- Eligible overpay sample: ", sum(panel$eligible_overpay, na.rm = TRUE), "."),
  paste0("- Captain-contract count (captains only): ", captain_contract_count$captain_contract_count, "; eligible overpay captains: ", captain_contract_count$captain_contract_count_eligible_overpay, "."),
  paste0("- Retention distribution: ", paste0(retention_distribution$retention_status, "=", retention_distribution$n, collapse = ", "), "."),
  paste0(
    "- Thin tier x trajectory buckets (n < 20): ",
    if (length(thin_bucket_labels) > 0) paste(thin_bucket_labels, collapse = "; ") else "none",
    "."
  ),
  "",
  "## Part 1 - Walk-year effect by archetype",
  paste0(
    "- Aggregate eligible walk-year effect is muted: mean post-signing TOI change = ",
    format_num(aggregate_walk$mean_post_signing_toi_change),
    ", median = ", format_num(aggregate_walk$median_post_signing_toi_change),
    "; mean points change = ", format_num(aggregate_walk$mean_post_signing_points_change),
    "."
  ),
  paste0(
    "- Aggregate walk-year trend delta (walk year minus expected from prior trend) = ",
    format_num(aggregate_walk$mean_walk_year_trend_delta),
    "; spike-above-trend share = ", format_pct(aggregate_walk$share_spike_above_trend),
    "."
  ),
  "- Buckets with the strongest post-signing drop (n >= 20):",
  paste0("  - ", bucket_drop_examples),
  "- Buckets with flat-to-positive post-signing outcomes (n >= 20):",
  paste0("  - ", if (length(bucket_hold_examples) > 0) bucket_hold_examples else "None at n >= 20."),
  if (nrow(walk_model_coefs) > 0) {
    paste0(
      "- Interpretable TOI-change model (post_signing_toi_change ~ tier + trajectory + age): age coefficient = ",
      format_num(walk_age_coef$estimate),
      " (p=", format_num(walk_age_coef$p_value, 4),
      "). Full coefficients: output/tables/walk_year_toi_change_model.csv."
    )
  } else {
    "- Interpretable TOI-change model was not estimated due to insufficient variation/sample."
  },
  "",
  "## Part 2 - Retention-overpay loyalty-tax test",
  paste0(
    "- same_team mean overpay_residual = ", format_num(ret_same$mean_overpay_residual),
    " (n=", ret_same$n, "), new_team mean = ", format_num(ret_new$mean_overpay_residual),
    " (n=", ret_new$n, ")."
  ),
  paste0(
    "- Mean difference (same_team - new_team) = ", format_num(ret_diff$mean_overpay_residual),
    ". Negative supports loyalty-tax; positive does not."
  ),
  if (nrow(retention_model_coefs) > 0 && nrow(ret_same_coef) == 1) {
    paste0(
      "- Tier-controlled model coefficient for same_team (vs new_team) = ",
      format_num(ret_same_coef$estimate),
      " (p=", format_num(ret_same_coef$p_value, 4),
      ")."
    )
  } else {
    "- Tier-controlled retention model was not estimated due to insufficient sample/variation."
  },
  "",
  "## Part 3 - Discount profile (descriptive)",
  paste0("- Positive-residual contracts in eligible set: ", discount_n, "."),
  paste0(
    "- Most common discount tier: ", discount_top_tier$level,
    " (", discount_top_tier$n, ", ", format_pct(discount_top_tier$share), ")."
  ),
  paste0(
    "- Most common discount trajectory: ", discount_top_traj$level,
    " (", discount_top_traj$n, ", ", format_pct(discount_top_traj$share), ")."
  ),
  paste0(
    "- Most common discount retention status: ", discount_top_retention$level,
    " (", discount_top_retention$n, ", ", format_pct(discount_top_retention$share), ")."
  ),
  paste0(
    "- Discount age profile: mean ", format_num(discount_age$age_mean),
    ", median ", format_num(discount_age$age_median),
    ", IQR [", format_num(discount_age$age_p25), ", ", format_num(discount_age$age_p75), "]."
  ),
  paste0(
    "- Material discount examples (cap-share >= ", material_cap_share_floor,
    "; games >= ", material_games_floor,
    " or TOI >= ", material_toi_floor,
    "): ",
    if (length(material_discount_examples) > 0) paste(material_discount_examples, collapse = ", ") else "none in current window",
    "."
  ),
  if (bergeron_in_data) {
    if (bergeron_in_discounts) {
      "- Recognizable discount captain check: Bergeron appears in the current discount set."
    } else {
      "- Recognizable discount captain check: Bergeron appears in the current data window but not in the current positive-residual discount set."
    }
  } else {
    "- Recognizable discount captain check: Bergeron is not present in the current contract window/source pull."
  },
  "",
  "## Part 4 - Captaincy lens (descriptive, thin sample)",
  paste0(
    "- Captain contracts mean overpay_residual = ", format_num(capt_summary_captain$mean_overpay_residual),
    " (n=", capt_summary_captain$n, "), non-captain mean = ",
    format_num(capt_summary_non$mean_overpay_residual),
    " (n=", capt_summary_non$n, ")."
  ),
  paste0(
    "- Captain contracts listed with residuals: ",
    if (length(captain_names) > 0) paste(captain_names, collapse = ", ") else "none",
    "."
  ),
  "- Caveat: captain sample is small; captaincy data is captains-only (no alternates); captaincy is association, not causation.",
  "",
  "## Part 5 - Overpay/discount extremes (raw vs material)",
  paste0("- Raw extremes include low-usage edge cases by design. Rows: ", nrow(overpay_extremes_raw), "."),
  paste0(
    "- Material lens floors used for story-ready ranking: cap-share >= ", material_cap_share_floor,
    ", and usage >= ", material_games_floor,
    " games or >= ", material_toi_floor,
    " TOI minutes. Rows: ", nrow(overpay_extremes_material), "."
  ),
  paste0(
    "- Material overpay examples: ",
    if (length(material_overpay_examples) > 0) paste(material_overpay_examples, collapse = ", ") else "none",
    "."
  ),
  paste0(
    "- Material discount examples: ",
    if (length(material_discount_examples) > 0) paste(material_discount_examples, collapse = ", ") else "none",
    "."
  ),
  "- Spot-check traces for hockey-sense sanity:",
  paste0(
    "  - Overpay trace: ",
    if (length(material_overpay_examples) > 0) material_overpay_examples[[1]] else "no material overpay example available",
    "."
  ),
  paste0(
    "  - Discount trace: ",
    if (length(material_discount_examples) > 0) material_discount_examples[[1]] else "no material discount example available",
    "."
  ),
  "",
  "## Honest limitations",
  "- Coverage is limited to contracts in the current data window. Some iconic historical deals predate the extracted contract source window and are narrative anchors, not panel rows.",
  "- Players without NHL performance records are out of scope because TOI outcomes cannot be computed.",
  "- Captaincy source is captains-only from Wikipedia captaincy histories (no alternates).",
  "- Intent is unmeasurable; this captures revealed outcomes (TOI delivered versus cap-share cost), not motivation.",
  "- Contract data: a GitHub-hosted community dataset snapshot from Chief-Zach Sports-Data, accessed through the contract source seam adapter in extraction. Current signing-year window is 2012 to 2025.",
  "- TOI is the value proxy and reflects coaching usage, not every dimension of player value.",
  "",
  "## Output files",
  "- output/tables/walk_year_effect_by_bucket.csv",
  "- output/tables/retention_overpay_comparison.csv",
  "- output/tables/discount_profile.csv",
  "- output/tables/captaincy_lens.csv",
  "- output/tables/overpay_extremes_raw.csv",
  "- output/tables/overpay_extremes_material.csv",
  "- output/tables/phase5_findings_summary.md"
)

findings_path <- file.path(out_dir, "phase5_findings_summary.md")
writeLines(findings_lines, findings_path)

message("Phase 5 analysis complete")
message(sprintf("- coverage tables saved to: %s", out_dir))
message("- core finding tables saved:")
message("  * walk_year_effect_by_bucket.csv")
message("  * retention_overpay_comparison.csv")
message("  * discount_profile.csv")
message("  * captaincy_lens.csv")
message("  * overpay_extremes_raw.csv")
message("  * overpay_extremes_material.csv")
message("- summary markdown saved: output/tables/phase5_findings_summary.md")