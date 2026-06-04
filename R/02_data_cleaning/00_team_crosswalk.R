# 00_team_crosswalk.R
# Shared team-name crosswalk utilities for Phase 2 cleaning scripts.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
  library(tibble)
})

normalize_team_name <- function(x) {
  stringr::str_squish(stringr::str_to_lower(as.character(x)))
}

get_team_crosswalk <- function() {
  tibble::tribble(
    ~team_name_full, ~team_abbr,
    "Anaheim Ducks", "ANA",
    "Arizona Coyotes", "ARI",
    "Phoenix Coyotes", "ARI",
    "Boston Bruins", "BOS",
    "Buffalo Sabres", "BUF",
    "Calgary Flames", "CGY",
    "Carolina Hurricanes", "CAR",
    "Chicago Blackhawks", "CHI",
    "Colorado Avalanche", "COL",
    "Columbus Blue Jackets", "CBJ",
    "Dallas Stars", "DAL",
    "Detroit Red Wings", "DET",
    "Edmonton Oilers", "EDM",
    "Florida Panthers", "FLA",
    "Los Angeles Kings", "LAK",
    "Minnesota Wild", "MIN",
    "Montreal Canadiens", "MTL",
    "Montr\u00e9al Canadiens", "MTL",
    "Nashville Predators", "NSH",
    "New Jersey Devils", "NJD",
    "New York Islanders", "NYI",
    "New York Rangers", "NYR",
    "Ottawa Senators", "OTT",
    "Philadelphia Flyers", "PHI",
    "Pittsburgh Penguins", "PIT",
    "San Jose Sharks", "SJS",
    "Seattle Kraken", "SEA",
    "St. Louis Blues", "STL",
    "Tampa Bay Lightning", "TBL",
    "Toronto Maple Leafs", "TOR",
    "Utah Hockey Club", "UTA",
    "Vancouver Canucks", "VAN",
    "Vegas Golden Knights", "VGK",
    "Washington Capitals", "WSH",
    "Winnipeg Jets", "WPG"
  ) %>%
    mutate(team_name_key = normalize_team_name(.data$team_name_full))
}

write_team_crosswalk <- function(output_path = here::here("data", "processed", "team_crosswalk.csv")) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  crosswalk <- get_team_crosswalk() %>%
    distinct(.data$team_name_key, .keep_all = TRUE) %>%
    arrange(.data$team_abbr, .data$team_name_full)

  readr::write_csv(crosswalk, output_path)
  output_path
}

if (sys.nframe() == 0) {
  output_path <- write_team_crosswalk()

  bios_path <- here::here("data", "raw", "nhlscraper_player_bios_raw.csv")
  spotrac_path <- here::here("data", "raw", "spotrac_contracts_raw.csv")
  wikipedia_path <- here::here("data", "raw", "wikipedia_captaincy_raw.csv")

  message("Team crosswalk QA summary")
  if (file.exists(bios_path)) {
    bios <- readr::read_csv(bios_path, show_col_types = FALSE)
    observed_abbr <- sort(unique(bios$team))
    mapped_abbr <- sort(unique(get_team_crosswalk()$team_abbr))
    missing_abbr <- setdiff(observed_abbr, mapped_abbr)

    message(sprintf("- unique abbreviations in bios: %s", length(observed_abbr)))
    message(sprintf(
      "- abbreviations missing from crosswalk: %s",
      ifelse(length(missing_abbr) == 0, "none", paste(missing_abbr, collapse = ", "))
    ))
  }

  if (file.exists(spotrac_path)) {
    spotrac <- readr::read_csv(spotrac_path, show_col_types = FALSE)
    missing_spotrac <- setdiff(
      sort(unique(normalize_team_name(spotrac$signing_team))),
      get_team_crosswalk()$team_name_key
    )
    message(sprintf(
      "- Contract-source signing_team names missing from crosswalk: %s",
      ifelse(length(missing_spotrac) == 0, "none", paste(missing_spotrac, collapse = ", "))
    ))
  }

  if (file.exists(wikipedia_path)) {
    wikipedia <- readr::read_csv(wikipedia_path, show_col_types = FALSE)
    missing_wikipedia <- setdiff(
      sort(unique(normalize_team_name(wikipedia$team))),
      get_team_crosswalk()$team_name_key
    )
    message(sprintf(
      "- Wikipedia team names missing from crosswalk: %s",
      ifelse(length(missing_wikipedia) == 0, "none", paste(missing_wikipedia, collapse = ", "))
    ))
  }

  message(sprintf("- saved: %s", output_path))
}
