# 04_build_analysis_panel.R
# Merge Phase 3 panel with Phase 4 engineered features into analysis-ready panel.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

panel_path <- here::here("data", "processed", "player_contract_panel.csv")
performance_features_path <- here::here("data", "processed", "contract_performance_features.csv")
overpay_features_path <- here::here("data", "processed", "contract_overpay_features.csv")
out_path <- here::here("data", "processed", "play_for_contract_analysis_panel.csv")

required_paths <- c(panel_path, performance_features_path, overpay_features_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop("Missing required input files:\n", paste0("- ", missing_paths, collapse = "\n"))
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

panel <- readr::read_csv(panel_path, show_col_types = FALSE)
performance_features <- readr::read_csv(performance_features_path, show_col_types = FALSE)
overpay_features <- readr::read_csv(overpay_features_path, show_col_types = FALSE)

analysis_panel <- panel %>%
  left_join(performance_features, by = "contract_id") %>%
  left_join(overpay_features, by = "contract_id") %>%
  mutate(
    eligible_walk_year =
      .data$retention_status != "entry" &
      !.data$is_extension &
      !.data$before_coverage_boundary &
      !is.na(.data$walk_year_toi_per_game),
    eligible_overpay = dplyr::coalesce(.data$eligible_overpay, FALSE)
  )

readr::write_csv(analysis_panel, out_path)

walk_year_eligible_n <- sum(analysis_panel$eligible_walk_year, na.rm = TRUE)
overpay_eligible_n <- sum(analysis_panel$eligible_overpay, na.rm = TRUE)

message("Phase 4 - final analysis panel summary")
message(sprintf("- rows in analysis panel: %s", nrow(analysis_panel)))
message(sprintf("- eligible_walk_year: %s", walk_year_eligible_n))
message(sprintf("- eligible_overpay: %s", overpay_eligible_n))
message("- trajectory distribution:")
print(analysis_panel %>% count(trajectory, name = "n") %>% arrange(desc(.data$n), .data$trajectory))
message("- tier distribution (eligible_walk_year only):")
print(
  analysis_panel %>%
    filter(.data$eligible_walk_year, !is.na(.data$tier)) %>%
    count(tier, name = "n") %>%
    arrange(desc(.data$n), .data$tier)
)
message(sprintf("- saved: %s", out_path))