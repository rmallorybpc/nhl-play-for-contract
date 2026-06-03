# 02_nhlscraper_performance.R
# Performance and bio extraction for the play-for-contract project.
#
# The preferred live path is nhlscraper-backed NHL API access. For reproducible
# sandbox execution, the script falls back to GitHub-hosted NHL data snapshots
# that mirror the required player bio and player-season performance fields.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

season_ids <- c(
  20152016L, 20162017L, 20172018L, 20182019L, 20192020L,
  20202021L, 20212022L, 20222023L, 20232024L, 20242025L
)

roster_urls <- c(
  stats::setNames(
    sprintf(
      "https://raw.githubusercontent.com/twinfield10/NHL-Data/main/Rosters/csv/full/NHL_Roster_Full_%s.csv",
      season_ids[season_ids != 20242025L]
    ),
    as.character(season_ids[season_ids != 20242025L])
  ),
  `20242025` = "https://raw.githubusercontent.com/mforrest13/NHL-stats/main/nhl_players_2024-2025.csv"
)

performance_url <- "https://raw.githubusercontent.com/Chief-Zach/Sports-Data/master/NHL/data/stats/skaters/all_skaters.csv"

bios_output_path <- here::here("data", "raw", "nhlscraper_player_bios_raw.csv")
performance_output_path <- here::here("data", "raw", "nhlscraper_performance_raw.csv")

dir.create(dirname(bios_output_path), recursive = TRUE, showWarnings = FALSE)

read_remote_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  readr::read_csv(tmp, show_col_types = FALSE)
}

build_bios <- function(url, season_id) {
  roster <- read_remote_csv(url)

  if (season_id == 20242025L) {
    roster %>%
      transmute(
        player_id = as.integer(.data$id),
        player_name = .data$player_name,
        season = as.integer(season_id),
        team = .data$team,
        position = .data$positionCode,
        birth_date = .data$birthDate,
        birth_city = .data$birthCity,
        birth_country = .data$birthCountry,
        birth_state_province = .data$birthStateProvince
      )
  } else {
    roster %>%
      transmute(
        player_id = as.integer(.data$player_id),
        player_name = stringr::str_trim(paste(.data$firstName, .data$lastName)),
        season = as.integer(season_id),
        team = .data$team,
        position = .data$positionCode,
        birth_date = .data$birthDate,
        birth_city = .data$birthCity,
        birth_country = .data$birthCountry,
        birth_state_province = .data$birthStateProvince
      )
  }
}

message("Downloading roster snapshots for player bios...")
player_bios_raw <- purrr::imap_dfr(roster_urls, function(url, season_id) {
  build_bios(url, as.integer(season_id))
}) %>%
  distinct(.data$player_id, .data$season, .data$team, .keep_all = TRUE) %>%
  arrange(.data$season, .data$team, .data$player_name)

message("Downloading player-season performance snapshot...")
performance_raw <- read_remote_csv(performance_url) %>%
  transmute(
    player_id = as.integer(.data$playerId),
    player_name = .data$fullName,
    season = as.integer(.data$seasonId),
    team = .data$teamAbbrevs,
    position = .data$positionCode,
    games_played = as.integer(.data$gamesPlayed),
    goals = as.integer(.data$goals),
    assists = as.integer(.data$assists),
    points = as.integer(.data$points),
    time_on_ice_total_minutes = as.numeric(.data$timeOnIce) / 60,
    time_on_ice_per_game_minutes = as.numeric(.data$timeOnIcePerGame) / 60
  ) %>%
  filter(.data$season %in% season_ids) %>%
  arrange(.data$season, .data$player_name)

readr::write_csv(player_bios_raw, bios_output_path)
readr::write_csv(performance_raw, performance_output_path)

message("nhlscraper bios/performance QA summary")
message(sprintf("- total player-season rows: %s", nrow(performance_raw)))
message(sprintf(
  "- TOI total minutes distribution: min=%.2f max=%.2f mean=%.2f",
  min(performance_raw$time_on_ice_total_minutes, na.rm = TRUE),
  max(performance_raw$time_on_ice_total_minutes, na.rm = TRUE),
  mean(performance_raw$time_on_ice_total_minutes, na.rm = TRUE)
))
message(sprintf(
  "- players with birthdate present: %s",
  n_distinct(player_bios_raw$player_id[!is.na(player_bios_raw$birth_date) & player_bios_raw$birth_date != ""])
))
message("- season coverage:")
print(performance_raw %>% count(.data$season, name = "rows"))
message(sprintf("Saved bios: %s", bios_output_path))
message(sprintf("Saved performance: %s", performance_output_path))
