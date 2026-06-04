# 02_clean_spotrac.R
# Clean extracted contract rows and reconcile names to player_id.

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
    last_name = stringr::word(name, -1)
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

get_colliding_name_keys <- function(identity_variants) {
  identity_variants %>%
    group_by(.data$name_key) %>%
    summarise(player_id_n = n_distinct(.data$player_id), .groups = "drop") %>%
    filter(.data$player_id_n > 1) %>%
    pull(.data$name_key)
}

build_player_era_lookup <- function(bios_skaters) {
  bios_skaters %>%
    group_by(.data$player_id) %>%
    summarise(
      min_season = min(.data$season, na.rm = TRUE),
      max_season = max(.data$season, na.rm = TRUE),
      .groups = "drop"
    )
}

build_player_team_lookup <- function(bios_skaters) {
  bios_skaters %>%
    distinct(.data$player_id, .data$season, .data$team)
}

resolve_collision_candidate <- function(target_name_key, target_signing_team_abbr, target_season, identity_variants, player_team_lookup, player_era_lookup) {
  candidates <- identity_variants %>%
    filter(.data$name_key == target_name_key) %>%
    distinct(.data$player_id)

  if (nrow(candidates) == 0) {
    return(list(player_id = NA_integer_, resolution = "collision_unresolved"))
  }

  candidate_scores <- candidates %>%
    left_join(player_era_lookup, by = "player_id") %>%
    mutate(
      era_match = !is.na(.data$min_season) & !is.na(.data$max_season) & target_season >= .data$min_season & target_season <= .data$max_season
    ) %>%
    rowwise() %>%
    mutate(
      team_season_match = any(
        player_team_lookup$player_id == .data$player_id &
          player_team_lookup$season == target_season &
          player_team_lookup$team == target_signing_team_abbr
      ),
      team_any_match = any(
        player_team_lookup$player_id == .data$player_id &
          player_team_lookup$team == target_signing_team_abbr
      )
    ) %>%
    ungroup() %>%
    mutate(
      score =
        ifelse(.data$team_season_match, 100L, 0L) +
        ifelse(.data$era_match, 10L, 0L) +
        ifelse(.data$team_any_match, 1L, 0L)
    ) %>%
    arrange(desc(.data$score), .data$player_id)

  if (nrow(candidate_scores) == 0) {
    return(list(player_id = NA_integer_, resolution = "collision_unresolved"))
  }

  top_score <- candidate_scores$score[[1]]
  top_rows <- candidate_scores %>% filter(.data$score == top_score)

  if (top_score > 0 && nrow(top_rows) == 1) {
    return(list(player_id = as.integer(top_rows$player_id[[1]]), resolution = "collision_team_era"))
  }

  list(player_id = NA_integer_, resolution = "collision_unresolved")
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
      last_name_key = normalize_name_key(.data$last_name),
      name_key_first_last = normalize_name_key(paste(.data$first_name, .data$last_name)),
      last_initial_key = stringr::str_sub(.data$first_name_key, 1, 1)
    )

  exact_candidates_full <- working %>%
    select(row_id, name_key) %>%
    left_join(
      identity_variants %>% select(player_id, name_key),
      by = "name_key",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(.data$player_id))

  exact_resolved_full <- exact_candidates_full %>%
    group_by(.data$row_id) %>%
    summarise(
      player_id = ifelse(n_distinct(.data$player_id) == 1, first(.data$player_id), NA_integer_),
      .groups = "drop"
    ) %>%
    mutate(match_status = ifelse(!is.na(.data$player_id), "exact", "ambiguous_exact"))

  exact_full_matched_row_ids <- exact_resolved_full %>%
    filter(!is.na(.data$player_id)) %>%
    pull(row_id)

  unmatched_after_exact_full <- working %>%
    filter(!(.data$row_id %in% exact_full_matched_row_ids))

  exact_candidates_first_last <- unmatched_after_exact_full %>%
    select(row_id, name_key_first_last) %>%
    left_join(
      identity_variants %>% select(player_id, name_key_first_last),
      by = "name_key_first_last",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(.data$player_id))

  exact_resolved_first_last <- exact_candidates_first_last %>%
    group_by(.data$row_id) %>%
    summarise(
      player_id = ifelse(n_distinct(.data$player_id) == 1, first(.data$player_id), NA_integer_),
      .groups = "drop"
    ) %>%
    mutate(match_status = ifelse(!is.na(.data$player_id), "exact_first_last", "ambiguous_exact_first_last"))

  exact_first_last_matched_row_ids <- exact_resolved_first_last %>%
    filter(!is.na(.data$player_id)) %>%
    pull(row_id)

  unmatched_after_exact <- unmatched_after_exact_full %>%
    filter(!(.data$row_id %in% exact_first_last_matched_row_ids))

  variant_candidates <- unmatched_after_exact %>%
    left_join(first_name_variants, by = c("first_name_key" = "name_a"), relationship = "many-to-many") %>%
    mutate(candidate_first = coalesce(.data$name_b, .data$first_name_key)) %>%
    mutate(candidate_key = stringr::str_squish(paste(.data$candidate_first, .data$last_name_key))) %>%
    select(row_id, candidate_key) %>%
    distinct() %>%
    left_join(
      identity_variants %>% select(player_id, name_key),
      by = c("candidate_key" = "name_key"),
      relationship = "many-to-many"
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
      by = "name_key_ascii",
      relationship = "many-to-many"
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
    left_join(exact_resolved_full %>% transmute(row_id, exact_player_id = player_id, exact_status = match_status), by = "row_id") %>%
    left_join(exact_resolved_first_last %>% transmute(row_id, exact_fl_player_id = player_id, exact_fl_status = match_status), by = "row_id") %>%
    left_join(variant_resolved %>% transmute(row_id, variant_player_id = player_id, variant_status = match_status), by = "row_id") %>%
    left_join(ascii_resolved %>% transmute(row_id, ascii_player_id = player_id, ascii_status = match_status), by = "row_id") %>%
    mutate(
      player_id = coalesce(.data$exact_player_id, .data$exact_fl_player_id, .data$variant_player_id, .data$ascii_player_id),
      match_status = case_when(
        !is.na(.data$exact_player_id) ~ .data$exact_status,
        !is.na(.data$exact_fl_player_id) ~ .data$exact_fl_status,
        !is.na(.data$variant_player_id) ~ .data$variant_status,
        !is.na(.data$ascii_player_id) ~ .data$ascii_status,
        TRUE ~ "unmatched"
      )
    )
}

seed_known_collision_overrides <- function(override_map) {
  seeded <- tibble::tribble(
    ~player_name, ~signing_team_abbr, ~season, ~player_id, ~override_note,
    "Sebastian Aho", "CAR", 20162017L, 8478427L, "manual collision override: CAR Sebastian Aho",
    "Sebastian Aho", "CAR", 20192020L, 8478427L, "manual collision override: CAR Sebastian Aho",
    "Sebastian Aho", "CAR", 20232024L, 8478427L, "manual collision override: CAR Sebastian Aho",
    "Sebastian Aho", "NYI", 20172018L, 8480222L, "manual collision override: NYI Sebastian Aho",
    "Sebastian Aho", "NYI", 20202021L, 8480222L, "manual collision override: NYI Sebastian Aho",
    "Sebastian Aho", "NYI", 20222023L, 8480222L, "manual collision override: NYI Sebastian Aho",
    "Sebastian Aho", "PIT", 20242025L, 8480222L, "manual collision override: PIT contract is NYI Sebastian Aho",
    "Elias Pettersson", "VAN", 20182019L, 8480012L, "manual collision override: VAN center Elias Pettersson",
    "Elias Pettersson", "VAN", 20212022L, 8480012L, "manual collision override: VAN center Elias Pettersson",
    "Elias Pettersson", "VAN", 20232024L, 8480012L, "manual collision override: VAN center Elias Pettersson",
    "Elias Pettersson", "VAN", 20242025L, 8480012L, "manual collision override: 8y/11.6M AAV 2024 VAN UFA deal"
  )

  override_map %>%
    full_join(
      seeded,
      by = c("player_name", "signing_team_abbr", "season"),
      suffix = c("", "_seed")
    ) %>%
    mutate(
      player_id = coalesce(.data$player_id_seed, .data$player_id),
      override_note = ifelse(!is.na(.data$player_id_seed), .data$override_note_seed, .data$override_note)
    ) %>%
    select(.data$player_name, .data$signing_team_abbr, .data$season, .data$player_id, .data$override_note) %>%
    arrange(.data$player_name, .data$season, .data$signing_team_abbr)
}

add_bios_presence_flags <- function(df, identity_variants) {
  bios_name_keys <- unique(identity_variants$name_key)
  bios_first_last_keys <- unique(identity_variants$name_key_first_last)
  bios_last_initial_keys <- identity_variants %>%
    mutate(
      first_name_key = stringr::word(.data$name_key_first_last, 1),
      last_name_key = stringr::word(.data$name_key_first_last, -1),
      last_initial_key = stringr::str_sub(.data$first_name_key, 1, 1),
      li_key = paste(.data$last_name_key, .data$last_initial_key, sep = "|")
    ) %>%
    pull(.data$li_key) %>%
    unique()

  df %>%
    mutate(
      appears_in_bios_exact = .data$name_key %in% bios_name_keys,
      appears_in_bios_first_last = .data$name_key_first_last %in% bios_first_last_keys,
      li_key = paste(.data$last_name_key, .data$last_initial_key, sep = "|"),
      appears_in_bios_last_initial = .data$li_key %in% bios_last_initial_keys,
      appears_in_bios_any = .data$appears_in_bios_exact | .data$appears_in_bios_first_last | .data$appears_in_bios_last_initial
    )
}

apply_collision_disambiguation <- function(matched, identity_variants, bios_skaters) {
  colliding_name_keys <- get_colliding_name_keys(identity_variants)
  player_era_lookup <- build_player_era_lookup(bios_skaters)
  player_team_lookup <- build_player_team_lookup(bios_skaters)

  working <- matched %>%
    mutate(
      original_player_id = .data$player_id,
      original_match_status = .data$match_status,
      was_colliding_name = .data$name_key %in% colliding_name_keys
    )

  collision_rows <- which(working$was_colliding_name)

  for (i in collision_rows) {
    resolved <- resolve_collision_candidate(
      target_name_key = working$name_key[[i]],
      target_signing_team_abbr = working$signing_team_abbr[[i]],
      target_season = working$season[[i]],
      identity_variants = identity_variants,
      player_team_lookup = player_team_lookup,
      player_era_lookup = player_era_lookup
    )

    working$player_id[[i]] <- resolved$player_id
    working$match_status[[i]] <- resolved$resolution
  }

  list(
    data = working,
    colliding_name_keys = colliding_name_keys,
    colliding_row_count = length(collision_rows),
    colliding_reassigned_count = sum(
      working$was_colliding_name &
        !is.na(working$player_id) &
        (is.na(working$original_player_id) | working$player_id != working$original_player_id)
    ),
    colliding_name_only_assigned_count = sum(
      working$was_colliding_name &
        working$match_status %in% c("exact", "first_name_variant", "accent_fallback")
    )
  )
}

build_or_update_override_map <- function(df, override_map_path) {
  unresolved <- df %>%
    filter(is.na(.data$player_id)) %>%
    transmute(
      player_name = .data$raw_name,
      signing_team_abbr = .data$signing_team_abbr,
      season = .data$season,
      player_id = as.integer(NA),
      override_note = ""
    ) %>%
    distinct()

  if (file.exists(override_map_path)) {
    existing <- readr::read_csv(override_map_path, show_col_types = FALSE) %>%
      mutate(
        player_name = as.character(.data$player_name),
        signing_team_abbr = as.character(.data$signing_team_abbr),
        season = as.integer(.data$season),
        player_id = as.integer(.data$player_id),
        override_note = as.character(.data$override_note)
      )

    new_rows <- unresolved %>%
      anti_join(existing, by = c("player_name", "signing_team_abbr", "season"))

    updated <- bind_rows(existing, new_rows) %>%
      distinct(.data$player_name, .data$signing_team_abbr, .data$season, .keep_all = TRUE) %>%
      arrange(.data$player_name, .data$season, .data$signing_team_abbr)
  } else {
    updated <- unresolved %>%
      arrange(.data$player_name, .data$season, .data$signing_team_abbr)
  }

  updated <- seed_known_collision_overrides(updated)

  readr::write_csv(updated, override_map_path)
  updated
}

apply_manual_overrides <- function(df, override_map) {
  active_overrides <- override_map %>%
    filter(!is.na(.data$player_id)) %>%
    transmute(
      player_name = .data$player_name,
      signing_team_abbr = .data$signing_team_abbr,
      season = as.integer(.data$season),
      override_player_id = as.integer(.data$player_id)
    ) %>%
    distinct()

  if (nrow(active_overrides) == 0) {
    return(list(data = df, overrides_applied = 0L))
  }

  out <- df %>%
    left_join(
      active_overrides,
      by = c("raw_name" = "player_name", "signing_team_abbr", "season"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      player_id_before_override = .data$player_id,
      player_id = coalesce(.data$override_player_id, .data$player_id),
      match_status = ifelse(!is.na(.data$override_player_id), "manual_override", .data$match_status)
    )

  overrides_applied <- sum(!is.na(out$override_player_id) & (is.na(out$player_id_before_override) | out$player_id_before_override != out$override_player_id))

  list(data = out, overrides_applied = overrides_applied)
}

spotrac_path <- here::here("data", "raw", "spotrac_contracts_raw.csv")
identity_path <- here::here("data", "processed", "identity_name_variants.csv")
first_name_variants_path <- here::here("data", "processed", "first_name_variants.csv")
bios_path <- here::here("data", "raw", "nhlscraper_player_bios_raw.csv")
out_path <- here::here("data", "processed", "spotrac_contracts_clean.csv")
unmatched_out_path <- here::here("data", "processed", "spotrac_unmatched_names.csv")
override_map_path <- here::here("data", "processed", "spotrac_manual_overrides.csv")

if (!file.exists(spotrac_path)) {
  stop("Missing contract raw file: ", spotrac_path)
}
if (!file.exists(identity_path) || !file.exists(first_name_variants_path)) {
  stop("Missing identity outputs. Run 01_build_identity_crosswalk.R first.")
}
if (!file.exists(bios_path)) {
  stop("Missing bios raw file: ", bios_path)
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

spotrac <- readr::read_csv(spotrac_path, show_col_types = FALSE)
identity_variants <- readr::read_csv(identity_path, show_col_types = FALSE)
first_name_variants <- readr::read_csv(first_name_variants_path, show_col_types = FALSE)
bios_skaters <- readr::read_csv(bios_path, show_col_types = FALSE) %>%
  filter(.data$position != "G") %>%
  transmute(
    player_id = as.integer(.data$player_id),
    season = as.integer(.data$season),
    team = as.character(.data$team)
  ) %>%
  distinct()
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
    by = c("signing_team_key" = "team_name_key"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    team_crosswalk %>% transmute(team_name_key, previous_team_abbr = team_abbr),
    by = c("previous_team_key" = "team_name_key"),
    relationship = "many-to-one"
  )

matched_first_pass <- apply_name_matching(
  df = spotrac_skaters,
  identity_variants = identity_variants,
  first_name_variants = first_name_variants,
  name_col = "player_name"
)

collision_pass <- apply_collision_disambiguation(
  matched = matched_first_pass,
  identity_variants = identity_variants,
  bios_skaters = bios_skaters
)

override_map <- build_or_update_override_map(collision_pass$data, override_map_path)
override_result <- apply_manual_overrides(collision_pass$data, override_map)

spotrac_final <- override_result$data
spotrac_final <- add_bios_presence_flags(spotrac_final, identity_variants)

spotrac_clean <- spotrac_final %>%
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

bios_scope_rows <- sum(spotrac_final$appears_in_bios_any)
bios_scope_matched_rows <- sum(!is.na(spotrac_clean$player_id) & spotrac_final$appears_in_bios_any)
bios_scope_match_rate <- ifelse(bios_scope_rows > 0, 100 * bios_scope_matched_rows / bios_scope_rows, NA_real_)

unmatched_names <- spotrac_clean %>%
  filter(is.na(.data$player_id)) %>%
  count(.data$player_name, sort = TRUE)

unmatched_names_in_bios <- spotrac_final %>%
  filter(is.na(.data$player_id), .data$appears_in_bios_any) %>%
  count(.data$raw_name, sort = TRUE)

readr::write_csv(unmatched_names, unmatched_out_path)

message("Contract cleaning QA summary")
message(sprintf("- rows after goalie filter: %s", total_rows))
message(sprintf("- matched to player_id: %s", matched_rows))
message(sprintf("- unmatched: %s", unmatched_rows))
message(sprintf("- contract-to-player_id match rate: %.2f%%", match_rate))
message(sprintf("- in-bios-contract match rate: %.2f%% (%s/%s)", bios_scope_match_rate, bios_scope_matched_rows, bios_scope_rows))
message("- match_status distribution:")
print(spotrac_clean %>% count(.data$match_status, sort = TRUE))
message(sprintf("- colliding name keys discovered in identity crosswalk: %s", length(collision_pass$colliding_name_keys)))
message(sprintf("- contract rows with colliding names: %s", collision_pass$colliding_row_count))
message(sprintf("- colliding-name rows re-assigned in disambiguation pass: %s", collision_pass$colliding_reassigned_count))
message(sprintf("- colliding-name rows still assigned by name-only statuses: %s", collision_pass$colliding_name_only_assigned_count))
message(sprintf("- manual overrides applied: %s", override_result$overrides_applied))
message(sprintf("- saved: %s", out_path))
message(sprintf("- saved unmatched list: %s", unmatched_out_path))
message(sprintf("- saved manual override map: %s", override_map_path))
message("- unmatched player names (manual review):")
if (nrow(unmatched_names) == 0) {
  message("none")
} else {
  print(unmatched_names)
}
message("- unmatched names that still appear in bios (true remaining failures):")
if (nrow(unmatched_names_in_bios) == 0) {
  message("none")
} else {
  print(unmatched_names_in_bios)
}
