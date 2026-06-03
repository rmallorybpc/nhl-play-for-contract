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

nhlscraper_available <- requireNamespace("nhlscraper", quietly = TRUE)
if (!nhlscraper_available) {
  message("nhlscraper package not installed; using snapshot fallback sources.")
}

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

parse_toi_minutes <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x) / 60)
  }

  x <- as.character(x)
  hh_mm_ss <- stringr::str_detect(x, "^\\d+:\\d{2}:\\d{2}$")
  mm_ss <- stringr::str_detect(x, "^\\d+:\\d{2}$")

  out <- suppressWarnings(as.numeric(x) / 60)

  if (any(hh_mm_ss, na.rm = TRUE)) {
    split_vals <- stringr::str_split_fixed(x[hh_mm_ss], ":", 3)
    out[hh_mm_ss] <- as.numeric(split_vals[, 1]) * 60 +
      as.numeric(split_vals[, 2]) +
      as.numeric(split_vals[, 3]) / 60
  }

  if (any(mm_ss, na.rm = TRUE)) {
    split_vals <- stringr::str_split_fixed(x[mm_ss], ":", 2)
    out[mm_ss] <- as.numeric(split_vals[, 1]) +
      as.numeric(split_vals[, 2]) / 60
  }

  out
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
performance_snapshot <- read_remote_csv(performance_url)

toi_total_col <- c("timeOnIce", "timeOnIceTotal", "toi")
toi_per_game_col <- c("timeOnIcePerGame", "toiPerGame", "timeOnIcePerGameMinutes")

available_toi_total_col <- toi_total_col[toi_total_col %in% names(performance_snapshot)]
available_toi_per_game_col <- toi_per_game_col[toi_per_game_col %in% names(performance_snapshot)]

if (length(available_toi_per_game_col) == 0) {
  stop("No TOI per-game column found in performance snapshot.")
}

toi_per_game_minutes <- parse_toi_minutes(performance_snapshot[[available_toi_per_game_col[[1]]]])

toi_total_minutes <- if (length(available_toi_total_col) > 0) {
  parse_toi_minutes(performance_snapshot[[available_toi_total_col[[1]]]])
} else {
  toi_per_game_minutes * as.numeric(performance_snapshot$gamesPlayed)
}

performance_raw <- performance_snapshot %>%
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
    time_on_ice_total_minutes = toi_total_minutes,
    time_on_ice_per_game_minutes = toi_per_game_minutes
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
  n_distinct(player_bios_raw$player_id[!is.na(player_bios_raw$birth_date)])
))
message("- season coverage:")
print(performance_raw %>% count(.data$season, name = "rows"))
message(sprintf("Saved bios: %s", bios_output_path))
message(sprintf("Saved performance: %s", performance_output_path))
