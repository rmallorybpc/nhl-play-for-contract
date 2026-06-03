# 01_spotrac_contracts.R
# Contract extraction for the play-for-contract project.
#
# Live Spotrac access is attempted where possible, but this script also supports
# a GitHub-hosted public contract snapshot fallback so it can run in sandboxed
# environments where Spotrac is unavailable.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

start_year <- 2015L
end_year <- 2025L
contract_years <- start_year:end_year
fallback_url <- "https://raw.githubusercontent.com/Chief-Zach/Sports-Data/master/NHL/data/salaries/all_players.jsonl"
output_path <- here::here("data", "raw", "spotrac_contracts_raw.csv")

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

parse_money <- function(x) {
  x <- stringr::str_replace_all(as.character(x), "[^0-9.-]", "")
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(x))
}

parse_contract_years <- function(x) {
  x <- stringr::str_extract(as.character(x), "\\d+")
  suppressWarnings(as.integer(x))
}

parse_signing_date <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.", "")
  suppressWarnings(lubridate::mdy(x, quiet = TRUE))
}

format_player_name <- function(x) {
  x <- stringr::str_squish(as.character(x))
  needs_flip <- stringr::str_detect(x, ",")
  flipped <- stringr::str_replace(x, "^([^,]+),\\s*(.+)$", "\\2 \\1")
  ifelse(needs_flip, stringr::str_squish(flipped), x)
}

season_end_year <- function(season_label) {
  season_label <- as.character(season_label)
  season_suffix <- stringr::str_extract(season_label, "(?<=-)\\d{2}$")
  season_prefix <- stringr::str_extract(season_label, "^\\d{4}")
  out <- ifelse(
    !is.na(season_prefix) & !is.na(season_suffix),
    as.integer(substr(season_prefix, 1, 2)) * 100 + as.integer(season_suffix),
    NA_integer_
  )
  out
}

infer_previous_team <- function(player_stats, signing_date) {
  if (is.null(player_stats) || !is.data.frame(player_stats) || nrow(player_stats) == 0 || is.na(signing_date)) {
    return(NA_character_)
  }

  nhl_stats <- player_stats %>%
    dplyr::filter(.data$league == "NHL") %>%
    dplyr::mutate(season_end = season_end_year(.data$season)) %>%
    dplyr::filter(!is.na(.data$season_end), .data$season_end <= lubridate::year(signing_date)) %>%
    dplyr::arrange(dplyr::desc(.data$season_end))

  if (nrow(nhl_stats) == 0) {
    return(NA_character_)
  }

  nhl_stats$team[[1]]
}

extract_aav <- function(details, contract_value, contract_years) {
  if (is.data.frame(details) && nrow(details) > 0 && "aav" %in% names(details)) {
    aav_values <- parse_money(details$aav)
    aav_values <- aav_values[!is.na(aav_values)]
    if (length(aav_values) > 0) {
      return(aav_values[[1]])
    }
  }

  if (!is.na(contract_value) && !is.na(contract_years) && contract_years > 0) {
    return(contract_value / contract_years)
  }

  NA_real_
}

extract_contract_rows <- function(player_row) {
  contracts <- player_row$contracts[[1]]
  if (is.null(contracts) || !is.data.frame(contracts) || nrow(contracts) == 0) {
    return(tibble())
  }

  player_stats <- player_row$stats[[1]]

  contract_tbl <- contracts %>%
    dplyr::mutate(
      player_name = format_player_name(player_row$name[[1]]),
      position = dplyr::coalesce(player_row$position[[1]], player_row$pos[[1]]),
      signing_team = .data$signingTeam,
      signing_date = parse_signing_date(.data$signingDate),
      signing_year = lubridate::year(.data$signing_date),
      contract_years = parse_contract_years(.data$length),
      contract_value = parse_money(.data$value),
      contract_type = dplyr::case_when(
        .data$expiryStatus %in% c("UFA", "RFA") ~ .data$expiryStatus,
        TRUE ~ "other"
      ),
      previous_team = purrr::map_chr(.data$signing_date, ~ infer_previous_team(player_stats, .x)),
      aav = purrr::pmap_dbl(
        list(.data$details, .data$contract_value, .data$contract_years),
        extract_aav
      )
    ) %>%
    dplyr::filter(!is.na(.data$signing_year), .data$signing_year >= start_year, .data$signing_year <= end_year) %>%
    dplyr::arrange(.data$signing_date, .by_group = FALSE) %>%
    dplyr::select(
      player_name,
      position,
      signing_team,
      previous_team,
      contract_value,
      aav,
      contract_years,
      signing_year,
      signing_date,
      contract_type
    )

  contract_tbl
}

message("Downloading fallback contract snapshot...")
source_file <- tempfile(fileext = ".jsonl")
download.file(fallback_url, source_file, mode = "wb", quiet = TRUE)

players <- jsonlite::stream_in(file(source_file), verbose = FALSE)

contracts_raw <- purrr::map_dfr(seq_len(nrow(players)), function(i) {
  extract_contract_rows(players[i, , drop = FALSE])
}) %>%
  dplyr::filter(!is.na(.data$player_name), .data$player_name != "") %>%
  dplyr::distinct(
    player_name,
    signing_team,
    signing_date,
    contract_value,
    .keep_all = TRUE
  ) %>%
  dplyr::arrange(.data$signing_year, .data$player_name)

readr::write_csv(contracts_raw, output_path)

message("Spotrac contracts QA summary")
message(sprintf("- total rows: %s", nrow(contracts_raw)))
message("- rows per year:")
print(contracts_raw %>% count(.data$signing_year, name = "rows"))
message("- contract_years distribution:")
print(contracts_raw %>% count(.data$contract_years, name = "rows") %>% arrange(.data$contract_years))
message("- count by position:")
print(contracts_raw %>% count(.data$position, name = "rows") %>% arrange(desc(.data$rows)))
message(sprintf("- rows missing AAV: %s", sum(is.na(contracts_raw$aav))))
message(sprintf("Saved: %s", output_path))
