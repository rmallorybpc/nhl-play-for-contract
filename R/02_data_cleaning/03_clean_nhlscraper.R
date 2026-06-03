# 03_clean_nhlscraper.R
# Clean and join nhlscraper bios and performance to a skaters-only panel.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

bios_path <- here::here("data", "raw", "nhlscraper_player_bios_raw.csv")
performance_path <- here::here("data", "raw", "nhlscraper_performance_raw.csv")
identity_path <- here::here("data", "processed", "identity_crosswalk.csv")
out_path <- here::here("data", "processed", "nhlscraper_skaters_clean.csv")

if (!file.exists(bios_path) || !file.exists(performance_path)) {
  stop("Missing nhlscraper raw files in data/raw.")
}
if (!file.exists(identity_path)) {
  stop("Missing identity crosswalk. Run 01_build_identity_crosswalk.R first.")
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

bios <- readr::read_csv(bios_path, show_col_types = FALSE)
performance <- readr::read_csv(performance_path, show_col_types = FALSE)
identity_crosswalk <- readr::read_csv(identity_path, show_col_types = FALSE)

bios_skaters <- bios %>%
  filter(.data$position != "G")

performance_skaters <- performance %>%
  filter(.data$position != "G")

bios_multi_team_rows <- bios_skaters %>%
  count(.data$player_id, .data$season, name = "team_rows") %>%
  filter(.data$team_rows > 1)

bios_unique <- bios_skaters %>%
  arrange(.data$player_id, .data$season, .data$team) %>%
  group_by(.data$player_id, .data$season) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    .data$player_id,
    .data$season,
    bio_player_name = .data$player_name,
    team = .data$team,
    position = .data$position,
    birth_date = .data$birth_date
  )

joined <- performance_skaters %>%
  select(
    .data$player_id,
    .data$season,
    games_played = .data$games_played,
    goals = .data$goals,
    assists = .data$assists,
    points = .data$points,
    time_on_ice_total_minutes = .data$time_on_ice_total_minutes,
    time_on_ice_per_game_minutes = .data$time_on_ice_per_game_minutes
  ) %>%
  left_join(bios_unique, by = c("player_id", "season")) %>%
  left_join(identity_crosswalk %>% select(.data$player_id, .data$canonical_name), by = "player_id") %>%
  mutate(canonical_name = coalesce(.data$canonical_name, .data$bio_player_name))

missing_bio <- joined %>%
  filter(is.na(.data$team) | is.na(.data$position))

nhlscraper_clean <- joined %>%
  filter(!is.na(.data$team), !is.na(.data$position)) %>%
  transmute(
    player_id = as.integer(.data$player_id),
    canonical_name = .data$canonical_name,
    season = as.integer(.data$season),
    team = .data$team,
    position = .data$position,
    birth_date = .data$birth_date,
    games_played = as.integer(.data$games_played),
    goals = as.integer(.data$goals),
    assists = as.integer(.data$assists),
    points = as.integer(.data$points),
    time_on_ice_total_minutes = as.numeric(.data$time_on_ice_total_minutes),
    time_on_ice_per_game_minutes = as.numeric(.data$time_on_ice_per_game_minutes)
  )

readr::write_csv(nhlscraper_clean, out_path)

message("nhlscraper clean QA summary")
message(sprintf("- bios raw rows: %s", nrow(bios)))
message(sprintf("- performance raw rows: %s", nrow(performance)))
message(sprintf("- bios skaters rows: %s", nrow(bios_skaters)))
message(sprintf("- performance skaters rows: %s", nrow(performance_skaters)))
message(sprintf("- bios player-season keys with multiple team rows: %s", nrow(bios_multi_team_rows)))
message(sprintf("- performance skater rows with no matching bio on player_id+season: %s", nrow(missing_bio)))
if (nrow(missing_bio) > 0) {
  message("- sample missing-bio rows:")
  print(missing_bio %>% select(.data$player_id, .data$season) %>% distinct() %>% head(20))
}
message(sprintf("- final cleaned skater rows: %s", nrow(nhlscraper_clean)))
message(sprintf(
  "- TOI total minutes range: %.2f to %.2f",
  min(nhlscraper_clean$time_on_ice_total_minutes, na.rm = TRUE),
  max(nhlscraper_clean$time_on_ice_total_minutes, na.rm = TRUE)
))
message(sprintf("- saved: %s", out_path))
