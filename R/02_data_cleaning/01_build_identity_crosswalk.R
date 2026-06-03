# 01_build_identity_crosswalk.R
# Build a canonical identity table keyed on player_id using nhlscraper bios.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(stringr)
  library(tidyr)
})

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
  first <- stringr::word(name, 1)
  last <- stringr::str_remove(name, "^\\S+\\s*")

  tibble::tibble(first_name = first, last_name = last)
}

get_first_name_variant_table <- function() {
  pairs <- tribble(
    ~name_a, ~name_b,
    "mitch", "mitchell",
    "alex", "alexander",
    "mike", "michael",
    "chris", "christopher",
    "matt", "matthew",
    "joe", "joseph",
    "nick", "nicholas",
    "zach", "zachary",
    "tom", "thomas",
    "dan", "daniel",
    "ben", "benjamin",
    "sam", "samuel",
    "rob", "robert",
    "bob", "robert",
    "jon", "jonathan",
    "dave", "david",
    "steve", "steven",
    "brad", "bradley",
    "pat", "patrick",
    "ty", "tyler",
    "will", "william",
    "bill", "william",
    "jim", "james",
    "jimmy", "james",
    "rick", "richard",
    "rich", "richard",
    "andy", "andrew",
    "nate", "nathan",
    "kris", "kristopher"
  )

  bind_rows(
    pairs,
    transmute(pairs, name_a = name_b, name_b = name_a)
  ) %>%
    distinct()
}

bios_path <- here::here("data", "raw", "nhlscraper_player_bios_raw.csv")
out_crosswalk_path <- here::here("data", "processed", "identity_crosswalk.csv")
out_variants_path <- here::here("data", "processed", "identity_name_variants.csv")
out_firstname_variants_path <- here::here("data", "processed", "first_name_variants.csv")

if (!file.exists(bios_path)) {
  stop("Missing bios raw file: ", bios_path)
}

dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

bios <- readr::read_csv(bios_path, show_col_types = FALSE)

identity_name_variants <- bios %>%
  filter(!is.na(.data$player_id), !is.na(.data$player_name), .data$player_name != "") %>%
  transmute(
    player_id = as.integer(.data$player_id),
    player_name = stringr::str_squish(.data$player_name),
    name_key = normalize_name_key(.data$player_name),
    name_key_ascii = normalize_name_key_ascii(.data$player_name)
  ) %>%
  distinct() %>%
  bind_cols(split_first_last(.$player_name)) %>%
  mutate(
    first_name_key = normalize_name_key(.data$first_name),
    last_name_key = normalize_name_key(.data$last_name)
  )

canonical_names <- bios %>%
  filter(!is.na(.data$player_id), !is.na(.data$player_name), .data$player_name != "") %>%
  transmute(
    player_id = as.integer(.data$player_id),
    player_name = stringr::str_squish(.data$player_name)
  ) %>%
  count(.data$player_id, .data$player_name, sort = TRUE) %>%
  group_by(.data$player_id) %>%
  arrange(desc(.data$n), .data$player_name, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(player_id = .data$player_id, canonical_name = .data$player_name)

identity_crosswalk <- identity_name_variants %>%
  group_by(.data$player_id) %>%
  summarise(
    all_name_variants = paste(sort(unique(.data$player_name)), collapse = " | "),
    normalized_keys = paste(sort(unique(.data$name_key)), collapse = " | "),
    normalized_ascii_keys = paste(sort(unique(.data$name_key_ascii)), collapse = " | "),
    .groups = "drop"
  ) %>%
  left_join(canonical_names, by = "player_id") %>%
  select(
    .data$player_id,
    .data$canonical_name,
    .data$all_name_variants,
    .data$normalized_keys,
    .data$normalized_ascii_keys
  ) %>%
  arrange(.data$player_id)

first_name_variants <- get_first_name_variant_table() %>%
  arrange(.data$name_a, .data$name_b)

readr::write_csv(identity_crosswalk, out_crosswalk_path)
readr::write_csv(identity_name_variants, out_variants_path)
readr::write_csv(first_name_variants, out_firstname_variants_path)

message("Identity crosswalk QA summary")
message(sprintf("- player_id rows: %s", nrow(identity_crosswalk)))
message(sprintf("- player_id with >1 observed name variant: %s", sum(str_detect(identity_crosswalk$all_name_variants, " \\| "))))
message(sprintf("- long name-variant rows: %s", nrow(identity_name_variants)))
message(sprintf("- first-name variant pairs (bidirectional): %s", nrow(first_name_variants)))
message(sprintf("- saved: %s", out_crosswalk_path))
message(sprintf("- saved: %s", out_variants_path))
message(sprintf("- saved: %s", out_firstname_variants_path))
