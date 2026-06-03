# 01_assemble_panel.R
# Assemble one-row-per-contract panel for analysis, with contract sequence,
# extension flags, and signing-time attributes.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(tidyr)
})

contracts_path <- here::here("data", "processed", "spotrac_contracts_clean.csv")
skaters_path <- here::here("data", "processed", "nhlscraper_skaters_clean.csv")
captaincy_path <- here::here("data", "processed", "wikipedia_captaincy_clean.csv")
identity_path <- here::here("data", "processed", "identity_crosswalk.csv")
out_path <- here::here("data", "processed", "player_contract_panel.csv")

required_paths <- c(contracts_path, skaters_path, captaincy_path, identity_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing required input files:\n", paste0("- ", missing_paths, collapse = "\n"))
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

contracts_raw <- readr::read_csv(contracts_path, show_col_types = FALSE)
skaters <- readr::read_csv(skaters_path, show_col_types = FALSE)
captaincy_raw <- readr::read_csv(captaincy_path, show_col_types = FALSE)
identity <- readr::read_csv(identity_path, show_col_types = FALSE)

contracts_with_player_id <- contracts_raw %>%
  filter(!is.na(.data$player_id))

dropped_no_player_id_n <- nrow(contracts_raw) - nrow(contracts_with_player_id)

players_with_performance <- skaters %>%
  filter(!is.na(.data$player_id), !is.na(.data$season)) %>%
  distinct(.data$player_id)

contracts_in_analysis <- contracts_with_player_id %>%
  semi_join(players_with_performance, by = "player_id")

dropped_no_performance_n <- nrow(contracts_with_player_id) - nrow(contracts_in_analysis)

player_bio <- skaters %>%
  arrange(.data$player_id, .data$season) %>%
  group_by(.data$player_id) %>%
  summarise(
    canonical_name_nhl = first_non_na(.data$canonical_name),
    birth_date = first_non_na(.data$birth_date),
    .groups = "drop"
  )

player_perf_bounds <- skaters %>%
  filter(!is.na(.data$player_id), !is.na(.data$season)) %>%
  group_by(.data$player_id) %>%
  summarise(
    first_perf_season = min(.data$season, na.rm = TRUE),
    .groups = "drop"
  )

walk_year_team_lookup <- skaters %>%
  filter(!is.na(.data$player_id), !is.na(.data$season), !is.na(.data$team)) %>%
  distinct(.data$player_id, .data$season, .data$team) %>%
  arrange(.data$player_id, .data$season, .data$team) %>%
  group_by(.data$player_id, .data$season) %>%
  summarise(
    walk_year_team = dplyr::first(.data$team),
    .groups = "drop"
  )

captaincy <- captaincy_raw %>%
  filter(!is.na(.data$player_id), !is.na(.data$season)) %>%
  distinct(.data$player_id, .data$season) %>%
  mutate(captaincy_status = "C")

performance_min_season <- min(skaters$season, na.rm = TRUE)

panel_full <- contracts_in_analysis %>%
  mutate(
    signing_date = as.Date(.data$signing_date),
    sort_signing_date = dplyr::coalesce(
      .data$signing_date,
      as.Date(sprintf("%d-07-01", .data$signing_year))
    )
  ) %>%
  arrange(.data$player_id, .data$sort_signing_date, .data$signing_year) %>%
  group_by(.data$player_id) %>%
  mutate(
    contract_sequence_number = dplyr::row_number(),
    first_observed = .data$contract_sequence_number == 1,
    prior_contract_signing_year = dplyr::lag(.data$signing_year),
    prior_contract_years = dplyr::lag(.data$contract_years),
    is_extension = dplyr::if_else(
      !is.na(.data$prior_contract_signing_year) &
        !is.na(.data$prior_contract_years) &
        .data$signing_year < (.data$prior_contract_signing_year + .data$prior_contract_years),
      TRUE,
      FALSE,
      missing = FALSE
    )
  ) %>%
  ungroup() %>%
  mutate(
    walk_year_season = (.data$signing_year - 1L) * 10000L + .data$signing_year,
    first_contract_season = .data$signing_year * 10000L + (.data$signing_year + 1L),
    before_coverage_boundary = .data$walk_year_season < performance_min_season
  ) %>%
  left_join(identity %>% select(player_id, canonical_name_identity = canonical_name), by = "player_id") %>%
  left_join(player_bio, by = "player_id") %>%
  left_join(player_perf_bounds, by = "player_id") %>%
  left_join(walk_year_team_lookup, by = c("player_id", "walk_year_season" = "season")) %>%
  left_join(captaincy, by = c("player_id", "walk_year_season" = "season")) %>%
  mutate(
    player_name = dplyr::coalesce(.data$canonical_name_identity, .data$canonical_name_nhl, .data$player_name),
    age_at_signing = dplyr::if_else(
      !is.na(.data$birth_date) & !is.na(.data$signing_date),
      floor(as.numeric(.data$signing_date - as.Date(.data$birth_date)) / 365.2425),
      NA_real_
    ),
    captaincy_status = dplyr::coalesce(.data$captaincy_status, "none"),
    previous_team_filled = dplyr::coalesce(.data$previous_team_abbr, .data$walk_year_team),
    has_prior_perf_season = !is.na(.data$first_perf_season) & .data$first_perf_season <= .data$walk_year_season,
    is_entry = .data$first_observed & !.data$has_prior_perf_season,
    retention_status = dplyr::case_when(
      .data$is_entry ~ "entry",
      !is.na(.data$previous_team_filled) & .data$signing_team_abbr == .data$previous_team_filled ~ "same_team",
      !is.na(.data$previous_team_filled) & .data$signing_team_abbr != .data$previous_team_filled ~ "new_team",
      TRUE ~ "unknown"
    ),
    signing_team = .data$signing_team_abbr,
    previous_team = .data$previous_team_filled
  ) %>%
  mutate(contract_id = dplyr::row_number())

panel <- panel_full %>%
  select(
    contract_id,
    player_id,
    player_name,
    position,
    contract_sequence_number,
    first_observed,
    signing_year,
    signing_date,
    walk_year_season,
    first_contract_season,
    signing_team,
    previous_team,
    contract_value,
    aav,
    contract_years,
    contract_type,
    age_at_signing,
    captaincy_status,
    retention_status,
    is_extension,
    prior_contract_signing_year,
    prior_contract_years,
    before_coverage_boundary
  )

readr::write_csv(panel, out_path)

extension_n <- sum(panel$is_extension, na.rm = TRUE)
extension_pct <- ifelse(nrow(panel) > 0, 100 * extension_n / nrow(panel), NA_real_)

extension_examples <- panel %>%
  filter(.data$is_extension) %>%
  mutate(
    prior_end_signing_year = .data$prior_contract_signing_year + .data$prior_contract_years
  ) %>%
  transmute(
    player_name = .data$player_name,
    signing_year = .data$signing_year,
    prior_contract_signing_year = .data$prior_contract_signing_year,
    prior_contract_years = .data$prior_contract_years,
    prior_end_signing_year = .data$prior_end_signing_year
  ) %>%
  arrange(.data$signing_year, .data$player_name) %>%
  head(10)

captain_n <- sum(panel$captaincy_status == "C", na.rm = TRUE)

retention_dist <- panel %>%
  count(retention_status, name = "n") %>%
  complete(retention_status = c("same_team", "new_team", "entry", "unknown"), fill = list(n = 0L)) %>%
  mutate(pct = 100 * .data$n / sum(.data$n))

inferred_previous_team_n <- panel_full %>%
  summarise(n = sum(is.na(.data$previous_team_abbr) & !is.na(.data$walk_year_team), na.rm = TRUE)) %>%
  pull(n)

unknown_retention_n <- retention_dist %>%
  filter(.data$retention_status == "unknown") %>%
  pull(.data$n)

age_min <- suppressWarnings(min(panel_full$age_at_signing, na.rm = TRUE))
age_max <- suppressWarnings(max(panel_full$age_at_signing, na.rm = TRUE))
age_outliers <- panel_full %>%
  filter(!is.na(.data$age_at_signing), (.data$age_at_signing < 18 | .data$age_at_signing > 42)) %>%
  select(contract_id, player_name, signing_date, birth_date, age_at_signing)

before_boundary_n <- sum(panel$before_coverage_boundary, na.rm = TRUE)

spotcheck_player <- panel %>%
  count(player_id, player_name, name = "contracts") %>%
  filter(.data$contracts >= 3) %>%
  arrange(desc(.data$contracts), .data$player_name) %>%
  slice(1)

spotcheck_rows <- if (nrow(spotcheck_player) == 1) {
  panel %>%
    filter(.data$player_id == spotcheck_player$player_id[[1]]) %>%
    arrange(.data$contract_sequence_number) %>%
    select(
      player_name,
      contract_sequence_number,
      signing_year,
      age_at_signing,
      retention_status,
      is_extension,
      first_observed
    )
} else {
  tibble::tibble()
}

message("Phase 3 panel assembly QA summary")
message(sprintf("- contracts raw: %s", nrow(contracts_raw)))
message(sprintf("- contracts dropped (missing player_id): %s", dropped_no_player_id_n))
message(sprintf("- contracts with player_id: %s", nrow(contracts_with_player_id)))
message(sprintf("- contracts dropped (no nhlscraper performance rows): %s", dropped_no_performance_n))
message(sprintf("- final panel rows: %s", nrow(panel)))
message(sprintf("- extension contracts: %s (%.2f%%)", extension_n, extension_pct))
if (nrow(extension_examples) > 0) {
  message("- extension examples (first 10):")
  print(extension_examples)
} else {
  message("- extension examples: none")
}
message(sprintf("- captaincy_status = C: %s", captain_n))
message("- retention status distribution:")
print(retention_dist)
message(sprintf("- previous_team inferred from walk-year nhlscraper team: %s", inferred_previous_team_n))
message(sprintf("- retention_status unknown after inference: %s", unknown_retention_n))
message(sprintf("- age_at_signing range: %.1f to %.1f", age_min, age_max))
message(sprintf("- age outliers (<18 or >42): %s", nrow(age_outliers)))
if (nrow(age_outliers) > 0) {
  print(age_outliers %>% head(20))
}
message(sprintf("- earliest performance season in nhlscraper: %s", performance_min_season))
message(sprintf("- contracts before coverage boundary: %s", before_boundary_n))
if (nrow(spotcheck_rows) > 0) {
  message("- spot-check player sequence:")
  print(spotcheck_rows)
} else {
  message("- spot-check player sequence: no player with >= 3 contracts found")
}
message(sprintf("- saved: %s", out_path))