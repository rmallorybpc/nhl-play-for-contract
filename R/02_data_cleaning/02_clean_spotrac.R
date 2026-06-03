# 02_clean_spotrac.R
# Clean Spotrac contracts and reconcile names to player_id.

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

is_goalie_only_position <- function(pos) {
  pos <- stringr::str_to_upper(stringr::str_replace_all(as.character(pos), "\\s+", ""))
  tokens <- stringr::str_split(pos, ",", simplify = FALSE)

  purrr::map_lgl(tokens, function(x) {
    x <- x[x != ""]
    length(x) > 0 && all(x == "G")
  })
}

apply_name_matching <- function(df, identity_variants, first_name_variants, name_col = "player_name") {
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
      exact_candidate_n = n_distinct(.data$player_id),
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
      variant_candidate_n = n_distinct(.data$player_id),
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
      ascii_candidate_n = n_distinct(.data$player_id),
      .groups = "drop"
    ) %>%
    mutate(match_status = ifelse(!is.na(.data$player_id), "accent_fallback", "ambiguous_ascii"))

  resolved <- working %>%
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

  resolved
}

spotrac_path <- here::here("data", "raw", "spotrac_contracts_raw.csv")
identity_path <- here::here("data", "processed", "identity_name_variants.csv")
first_name_variants_path <- here::here("data", "processed", "first_name_variants.csv")
out_path <- here::here("data", "processed", "spotrac_contracts_clean.csv")
unmatched_out_path <- here::here("data", "processed", "spotrac_unmatched_names.csv")

if (!file.exists(spotrac_path)) {
  stop("Missing Spotrac raw file: ", spotrac_path)
}
if (!file.exists(identity_path) || !file.exists(first_name_variants_path)) {
  stop("Missing identity outputs. Run 01_build_identity_crosswalk.R first.")
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

spotrac <- readr::read_csv(spotrac_path, show_col_types = FALSE)
identity_variants <- readr::read_csv(identity_path, show_col_types = FALSE)
first_name_variants <- readr::read_csv(first_name_variants_path, show_col_types = FALSE)
team_crosswalk <- get_team_crosswalk()

spotrac_skaters <- spotrac %>%
  filter(!is_goalie_only_position(.data$position)) %>%
  mutate(
    season_start = as.integer(.data$signing_year),
    season = as.integer(.data$signing_year * 10000L + (.data$signing_year + 1L)),
    signing_team_key = normalize_team_name(.data$signing_team),
    previous_team_key = normalize_team_name(.data$previous_team)
  ) %>%
  left_join(
    team_crosswalk %>% transmute(team_name_key, signing_team_abbr = team_abbr),
    by = c("signing_team_key" = "team_name_key")
  ) %>%
  left_join(
    team_crosswalk %>% transmute(team_name_key, previous_team_abbr = team_abbr),
    by = c("previous_team_key" = "team_name_key")
  )

matched <- apply_name_matching(
  df = spotrac_skaters,
  identity_variants = identity_variants,
  first_name_variants = first_name_variants,
  name_col = "player_name"
)

spotrac_clean <- matched %>%
  transmute(
    player_id = as.integer(.data$player_id),
    player_name = .data$raw_name,
    match_status = .data$match_status,
    position = .data$position,
    signing_team = .data$signing_team,
    signing_team_abbr = .data$signing_team_abbr,
    previous_team = .data$previous_team,
    previous_team_abbr = .data$previous_team_abbr,
    contract_value = .data$contract_value,
    aav = .data$aav,
    contract_years = .data$contract_years,
    signing_year = as.integer(.data$signing_year),
    season_start = as.integer(.data$season_start),
    season = as.integer(.data$season),
    signing_date = .data$signing_date,
    contract_type = .data$contract_type
  )

readr::write_csv(spotrac_clean, out_path)

total_rows <- nrow(spotrac_clean)
matched_rows <- sum(!is.na(spotrac_clean$player_id))
unmatched_rows <- sum(is.na(spotrac_clean$player_id))
match_rate <- ifelse(total_rows > 0, 100 * matched_rows / total_rows, NA_real_)

unmatched_names <- spotrac_clean %>%
  filter(is.na(.data$player_id)) %>%
  count(.data$player_name, sort = TRUE)

readr::write_csv(unmatched_names, unmatched_out_path)

message("Spotrac cleaning QA summary")
message(sprintf("- rows after goalie filter: %s", total_rows))
message(sprintf("- matched to player_id: %s", matched_rows))
message(sprintf("- unmatched: %s", unmatched_rows))
message(sprintf("- Spotrac-to-player_id match rate: %.2f%%", match_rate))
message("- match_status distribution:")
print(spotrac_clean %>% count(.data$match_status, sort = TRUE))
message("- unmatched player names (manual review):")
if (nrow(unmatched_names) == 0) {
  message("none")
} else {
  print(unmatched_names)
}
message(sprintf("- saved: %s", out_path))
message(sprintf("- saved unmatched list: %s", unmatched_out_path))
