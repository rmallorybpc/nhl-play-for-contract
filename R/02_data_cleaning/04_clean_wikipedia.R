# 04_clean_wikipedia.R
# Clean Wikipedia captaincy data and reconcile captain names to player_id.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
  library(tidyr)
})

source(here::here("R", "02_data_cleaning", "00_team_crosswalk.R"))

normalize_name_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_squish()
}

normalize_name_key_ascii <- function(x) {
  x %>%
    normalize_name_key() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
}

split_first_last <- function(name) {
  name <- stringr::str_squish(as.character(name))
  tibble::tibble(
    first_name = stringr::word(name, 1),
    last_name = stringr::str_remove(name, "^\\S+\\s*")
  )
}

parse_wiki_season <- function(x) {
  start_year <- suppressWarnings(as.integer(stringr::str_extract(as.character(x), "^\\d{4}")))
  ifelse(is.na(start_year), NA_integer_, start_year * 10000L + (start_year + 1L))
}

apply_name_matching <- function(df, identity_variants, first_name_variants, name_col = "captain_name") {
  working <- df %>%
    mutate(
      row_id = row_number(),
      raw_name = stringr::str_squish(.data[[name_col]]),
      name_key = normalize_name_key(.data$raw_name),
      name_key_ascii = normalize_name_key_ascii(.data$raw_name)
    ) %>%
    bind_cols(split_first_last(.$raw_name)) %>%
    mutate(
      first_name_key = normalize_name_key(.data$first_name),
      last_name_key = normalize_name_key(.data$last_name)
    )

  exact_candidates <- working %>%
    select(row_id, name_key) %>%
    left_join(
      identity_variants %>% select(player_id, name_key),
      by = "name_key"
    ) %>%
    filter(!is.na(.data$player_id))

  exact_resolved <- exact_candidates %>%
    group_by(.data$row_id) %>%
    summarise(
      player_id = ifelse(n_distinct(.data$player_id) == 1, first(.data$player_id), NA_integer_),
      .groups = "drop"
    ) %>%
    mutate(match_status = ifelse(!is.na(.data$player_id), "exact", "ambiguous_exact"))

  exact_matched_row_ids <- exact_resolved %>%
    filter(!is.na(.data$player_id)) %>%
    pull(row_id)

  unmatched_after_exact <- working %>%
    filter(!(.data$row_id %in% exact_matched_row_ids))

  variant_candidates <- unmatched_after_exact %>%
    left_join(first_name_variants, by = c("first_name_key" = "name_a")) %>%
    mutate(candidate_first = coalesce(.data$name_b, .data$first_name_key)) %>%
    mutate(candidate_key = stringr::str_squish(paste(.data$candidate_first, .data$last_name_key))) %>%
    select(row_id, candidate_key) %>%
    distinct() %>%
    left_join(
      identity_variants %>% select(player_id, name_key),
      by = c("candidate_key" = "name_key")
    ) %>%
    filter(!is.na(.data$player_id))

  variant_resolved <- variant_candidates %>%
    group_by(.data$row_id) %>%
    summarise(
      player_id = ifelse(n_distinct(.data$player_id) == 1, first(.data$player_id), NA_integer_),
      .groups = "drop"
    ) %>%
    mutate(match_status = ifelse(!is.na(.data$player_id), "first_name_variant", "ambiguous_variant"))

  variant_matched_row_ids <- variant_resolved %>%
    filter(!is.na(.data$player_id)) %>%
    pull(row_id)

  unmatched_after_variant <- unmatched_after_exact %>%
    filter(!(.data$row_id %in% variant_matched_row_ids))

  ascii_candidates <- unmatched_after_variant %>%
    select(row_id, name_key_ascii) %>%
    left_join(
      identity_variants %>% select(player_id, name_key_ascii),
      by = "name_key_ascii"
    ) %>%
    filter(!is.na(.data$player_id))

  ascii_resolved <- ascii_candidates %>%
    group_by(.data$row_id) %>%
    summarise(
      player_id = ifelse(n_distinct(.data$player_id) == 1, first(.data$player_id), NA_integer_),
      .groups = "drop"
    ) %>%
    mutate(match_status = ifelse(!is.na(.data$player_id), "accent_fallback", "ambiguous_ascii"))

  working %>%
    left_join(exact_resolved %>% transmute(row_id, exact_player_id = player_id, exact_status = match_status), by = "row_id") %>%
    left_join(variant_resolved %>% transmute(row_id, variant_player_id = player_id, variant_status = match_status), by = "row_id") %>%
    left_join(ascii_resolved %>% transmute(row_id, ascii_player_id = player_id, ascii_status = match_status), by = "row_id") %>%
    mutate(
      player_id = coalesce(.data$exact_player_id, .data$variant_player_id, .data$ascii_player_id),
      match_status = case_when(
        !is.na(.data$exact_player_id) ~ .data$exact_status,
        !is.na(.data$variant_player_id) ~ .data$variant_status,
        !is.na(.data$ascii_player_id) ~ .data$ascii_status,
        TRUE ~ "unmatched"
      )
    )
}

wikipedia_path <- here::here("data", "raw", "wikipedia_captaincy_raw.csv")
identity_path <- here::here("data", "processed", "identity_name_variants.csv")
first_name_variants_path <- here::here("data", "processed", "first_name_variants.csv")
out_path <- here::here("data", "processed", "wikipedia_captaincy_clean.csv")
unmatched_out_path <- here::here("data", "processed", "wikipedia_unmatched_names.csv")

if (!file.exists(wikipedia_path)) {
  stop("Missing Wikipedia raw file: ", wikipedia_path)
}
if (!file.exists(identity_path) || !file.exists(first_name_variants_path)) {
  stop("Missing identity outputs. Run 01_build_identity_crosswalk.R first.")
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

wikipedia <- readr::read_csv(wikipedia_path, show_col_types = FALSE)
identity_variants <- readr::read_csv(identity_path, show_col_types = FALSE)
first_name_variants <- readr::read_csv(first_name_variants_path, show_col_types = FALSE)
team_crosswalk <- get_team_crosswalk()

wikipedia_standardized <- wikipedia %>%
  mutate(
    season = parse_wiki_season(.data$season),
    team_key = normalize_team_name(.data$team)
  ) %>%
  left_join(
    team_crosswalk %>% transmute(team_name_key, team_abbr),
    by = c("team_key" = "team_name_key")
  )

matched <- apply_name_matching(
  df = wikipedia_standardized,
  identity_variants = identity_variants,
  first_name_variants = first_name_variants,
  name_col = "captain_name"
)

wikipedia_clean <- matched %>%
  transmute(
    player_id = as.integer(.data$player_id),
    captain_name = .data$raw_name,
    match_status = .data$match_status,
    team = .data$team,
    team_abbr = .data$team_abbr,
    season = as.integer(.data$season),
    source_page = .data$source_page,
    alternate_data_available = .data$alternate_data_available
  )

readr::write_csv(wikipedia_clean, out_path)

matched_rows <- sum(!is.na(wikipedia_clean$player_id))
unmatched_rows <- sum(is.na(wikipedia_clean$player_id))

unmatched_names <- wikipedia_clean %>%
  filter(is.na(.data$player_id)) %>%
  count(.data$captain_name, sort = TRUE)

readr::write_csv(unmatched_names, unmatched_out_path)

message("Wikipedia cleaning QA summary")
message(sprintf("- total captain-season rows: %s", nrow(wikipedia_clean)))
message(sprintf("- matched to player_id: %s", matched_rows))
message(sprintf("- unmatched: %s", unmatched_rows))
message("- unmatched captain names:")
if (nrow(unmatched_names) == 0) {
  message("none")
} else {
  print(unmatched_names)
}
message(sprintf("- saved: %s", out_path))
message(sprintf("- saved unmatched list: %s", unmatched_out_path))
