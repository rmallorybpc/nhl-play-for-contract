# 02_performance_windows.R
# Build walk-year, post-signing delivery, trajectory, and tier features.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
  library(tidyr)
})

panel_path <- here::here("data", "processed", "player_contract_panel.csv")
skaters_path <- here::here("data", "processed", "nhlscraper_skaters_clean.csv")
out_path <- here::here("data", "processed", "contract_performance_features.csv")

required_paths <- c(panel_path, skaters_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing required input files:\n", paste0("- ", missing_paths, collapse = "\n"))
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

season_step <- 10001L

season_offset <- function(season, n) {
  season - as.integer(n) * season_step
}

season_sequence <- function(first_season, n_years) {
  if (is.na(first_season) || is.na(n_years) || n_years <= 0) {
    return(integer(0))
  }
  first_season + (seq_len(as.integer(n_years)) - 1L) * season_step
}

position_group <- function(x) {
  x_up <- toupper(dplyr::coalesce(x, ""))
  dplyr::case_when(
    stringr::str_detect(x_up, "D") ~ "defense",
    stringr::str_detect(x_up, "C|LW|RW|W|F|L|R") ~ "forward",
    TRUE ~ NA_character_
  )
}

panel <- readr::read_csv(panel_path, show_col_types = FALSE)
skaters_raw <- readr::read_csv(skaters_path, show_col_types = FALSE)

# Collapse any multi-team season rows to one player-season record.
skaters <- skaters_raw %>%
  filter(!is.na(.data$player_id), !is.na(.data$season)) %>%
  mutate(
    position_group = position_group(.data$position),
    games_played = dplyr::coalesce(.data$games_played, 0),
    goals = dplyr::coalesce(.data$goals, 0),
    assists = dplyr::coalesce(.data$assists, 0),
    points = dplyr::coalesce(.data$points, 0),
    time_on_ice_total_minutes = dplyr::coalesce(.data$time_on_ice_total_minutes, 0)
  ) %>%
  arrange(.data$player_id, .data$season, .data$team) %>%
  group_by(.data$player_id, .data$season) %>%
  summarise(
    position_group = dplyr::first(stats::na.omit(.data$position_group)),
    games_played = sum(.data$games_played, na.rm = TRUE),
    goals = sum(.data$goals, na.rm = TRUE),
    assists = sum(.data$assists, na.rm = TRUE),
    points = sum(.data$points, na.rm = TRUE),
    time_on_ice_total_minutes = sum(.data$time_on_ice_total_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    toi_per_game = dplyr::if_else(.data$games_played > 0, .data$time_on_ice_total_minutes / .data$games_played, NA_real_),
    points_per_game = dplyr::if_else(.data$games_played > 0, .data$points / .data$games_played, NA_real_)
  )

walk_year_lookup <- skaters %>%
  select(
    player_id,
    season,
    walk_year_position_group = position_group,
    walk_year_toi_per_game = toi_per_game,
    walk_year_toi_total = time_on_ice_total_minutes,
    walk_year_games = games_played,
    walk_year_goals = goals,
    walk_year_assists = assists,
    walk_year_points = points,
    walk_year_points_per_game = points_per_game
  )

contracts_with_walk <- panel %>%
  left_join(
    walk_year_lookup,
    by = c("player_id", "walk_year_season" = "season")
  )

post_signing_features <- panel %>%
  rowwise() %>%
  mutate(
    post_signing_target_seasons = list(season_sequence(.data$first_contract_season, .data$contract_years))
  ) %>%
  ungroup() %>%
  select(contract_id, player_id, post_signing_target_seasons) %>%
  tidyr::unnest_longer(post_signing_target_seasons, values_to = "season") %>%
  rename(post_season = season) %>%
  left_join(
    skaters %>%
      select(
        player_id,
        season,
        toi_per_game,
        points_per_game
      ),
    by = c("player_id", "post_season" = "season")
  ) %>%
  group_by(.data$contract_id) %>%
  summarise(
    post_signing_seasons_observed = sum(!is.na(.data$toi_per_game) | !is.na(.data$points_per_game)),
    post_signing_avg_toi_per_game = dplyr::if_else(
      sum(!is.na(.data$toi_per_game)) > 0,
      mean(.data$toi_per_game, na.rm = TRUE),
      NA_real_
    ),
    post_signing_avg_points_per_game = dplyr::if_else(
      sum(!is.na(.data$points_per_game)) > 0,
      mean(.data$points_per_game, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

trajectory_threshold_minutes <- 0.35

trajectory_features <- panel %>%
  rowwise() %>%
  mutate(
    prior_seasons = list(c(
      season_offset(.data$walk_year_season, 1L),
      season_offset(.data$walk_year_season, 2L),
      season_offset(.data$walk_year_season, 3L)
    ))
  ) %>%
  ungroup() %>%
  select(contract_id, player_id, walk_year_season, prior_seasons) %>%
  tidyr::unnest_longer(prior_seasons, values_to = "season") %>%
  left_join(
    skaters %>%
      select(player_id, season, toi_per_game, points_per_game),
    by = c("player_id", "season")
  ) %>%
  filter(!is.na(.data$toi_per_game)) %>%
  arrange(.data$contract_id, .data$season) %>%
  group_by(.data$contract_id) %>%
  summarise(
    trajectory_prior_seasons_found = n(),
    trajectory_toi_slope = if (n() >= 2) {
      stats::coef(stats::lm(toi_per_game ~ season, data = dplyr::pick(season, toi_per_game)))[["season"]] * season_step
    } else {
      NA_real_
    },
    trajectory_points_slope = if (n() >= 2) {
      stats::coef(stats::lm(points_per_game ~ season, data = dplyr::pick(season, points_per_game)))[["season"]] * season_step
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    trajectory = dplyr::case_when(
      .data$trajectory_prior_seasons_found < 2 ~ "insufficient_history",
      .data$trajectory_toi_slope >= trajectory_threshold_minutes ~ "rising",
      .data$trajectory_toi_slope <= -trajectory_threshold_minutes ~ "declining",
      TRUE ~ "stable"
    )
  )

tier_table <- skaters %>%
  filter(!is.na(.data$position_group), !is.na(.data$toi_per_game)) %>%
  group_by(.data$season, .data$position_group) %>%
  mutate(tier_toi_percentile = dplyr::percent_rank(.data$toi_per_game)) %>%
  ungroup() %>%
  mutate(
    tier = dplyr::case_when(
      .data$tier_toi_percentile > (2 / 3) ~ "top",
      .data$tier_toi_percentile > (1 / 3) ~ "middle",
      TRUE ~ "fringe"
    )
  ) %>%
  select(player_id, season, position_group, tier_toi_percentile, tier)

features <- contracts_with_walk %>%
  left_join(post_signing_features, by = "contract_id") %>%
  left_join(trajectory_features, by = "contract_id") %>%
  left_join(
    tier_table,
    by = c(
      "player_id",
      "walk_year_season" = "season",
      "walk_year_position_group" = "position_group"
    )
  ) %>%
  mutate(
    post_signing_seasons_observed = dplyr::coalesce(.data$post_signing_seasons_observed, 0L),
    contract_complete = .data$post_signing_seasons_observed >= .data$contract_years,
    post_signing_toi_change = .data$post_signing_avg_toi_per_game - .data$walk_year_toi_per_game,
    post_signing_points_change = .data$post_signing_avg_points_per_game - .data$walk_year_points_per_game,
    trajectory = dplyr::coalesce(.data$trajectory, "insufficient_history")
  ) %>%
  select(
    contract_id,
    walk_year_position_group,
    walk_year_toi_per_game,
    walk_year_toi_total,
    walk_year_games,
    walk_year_goals,
    walk_year_assists,
    walk_year_points,
    walk_year_points_per_game,
    post_signing_seasons_observed,
    post_signing_avg_toi_per_game,
    post_signing_avg_points_per_game,
    contract_complete,
    post_signing_toi_change,
    post_signing_points_change,
    trajectory_prior_seasons_found,
    trajectory_toi_slope,
    trajectory_points_slope,
    trajectory,
    tier_toi_percentile,
    tier
  )

readr::write_csv(features, out_path)

message("Phase 4 - windows/trajectory/tier summary")
message(sprintf("- contracts processed: %s", nrow(panel)))
message(sprintf("- walk-year TOI available: %s", sum(!is.na(features$walk_year_toi_per_game))))
message(sprintf("- post-signing with >=1 observed season: %s", sum(features$post_signing_seasons_observed >= 1)))
message(sprintf("- contract_complete TRUE: %s", sum(features$contract_complete, na.rm = TRUE)))
message(sprintf("- trajectory slope threshold (minutes/game/season): %.2f", trajectory_threshold_minutes))
message("- trajectory distribution:")
print(features %>% count(trajectory, name = "n") %>% arrange(desc(.data$n), .data$trajectory))
message("- tier distribution (non-missing walk-year percentile only):")
print(features %>% filter(!is.na(.data$tier)) %>% count(tier, name = "n") %>% arrange(desc(.data$n), .data$tier))
message(sprintf("- saved: %s", out_path))