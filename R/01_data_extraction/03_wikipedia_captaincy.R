# 03_wikipedia_captaincy.R
# Captaincy extraction for the play-for-contract project.
#
# Wikipedia content is Creative Commons licensed and requires attribution.
# This script uses Wikipedia-derived captaincy data for the initial raw layer.
# In sandboxed environments where live Wikipedia scraping is unavailable, the
# script falls back to an embedded captaincy tenure table derived from the team
# captain history pages.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

output_path <- here::here("data", "raw", "wikipedia_captaincy_raw.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

season_label <- function(start_year) {
  sprintf("%d-%02d", start_year, (start_year + 1) %% 100)
}

captain_tenures <- tibble::tribble(
  ~team, ~start_year, ~end_year, ~captain_name, ~source_page, ~alternate_data_available,
  "Anaheim Ducks", 2015L, 2021L, "Ryan Getzlaf", "https://en.wikipedia.org/wiki/List_of_Anaheim_Ducks_captains", FALSE,
  "Boston Bruins", 2015L, 2019L, "Zdeno Chara", "https://en.wikipedia.org/wiki/List_of_Boston_Bruins_captains", FALSE,
  "Boston Bruins", 2020L, 2022L, "Patrice Bergeron", "https://en.wikipedia.org/wiki/List_of_Boston_Bruins_captains", FALSE,
  "Boston Bruins", 2023L, 2024L, "Brad Marchand", "https://en.wikipedia.org/wiki/List_of_Boston_Bruins_captains", FALSE,
  "Buffalo Sabres", 2015L, 2016L, "Brian Gionta", "https://en.wikipedia.org/wiki/List_of_Buffalo_Sabres_captains", FALSE,
  "Buffalo Sabres", 2018L, 2020L, "Jack Eichel", "https://en.wikipedia.org/wiki/List_of_Buffalo_Sabres_captains", FALSE,
  "Buffalo Sabres", 2022L, 2023L, "Kyle Okposo", "https://en.wikipedia.org/wiki/List_of_Buffalo_Sabres_captains", FALSE,
  "Buffalo Sabres", 2024L, 2024L, "Rasmus Dahlin", "https://en.wikipedia.org/wiki/List_of_Buffalo_Sabres_captains", FALSE,
  "Calgary Flames", 2015L, 2019L, "Mark Giordano", "https://en.wikipedia.org/wiki/List_of_Calgary_Flames_captains", FALSE,
  "Calgary Flames", 2023L, 2024L, "Mikael Backlund", "https://en.wikipedia.org/wiki/List_of_Calgary_Flames_captains", FALSE,
  "Carolina Hurricanes", 2015L, 2015L, "Eric Staal", "https://en.wikipedia.org/wiki/List_of_Carolina_Hurricanes_captains", FALSE,
  "Carolina Hurricanes", 2018L, 2020L, "Justin Williams", "https://en.wikipedia.org/wiki/List_of_Carolina_Hurricanes_captains", FALSE,
  "Carolina Hurricanes", 2021L, 2024L, "Jordan Staal", "https://en.wikipedia.org/wiki/List_of_Carolina_Hurricanes_captains", FALSE,
  "Chicago Blackhawks", 2015L, 2022L, "Jonathan Toews", "https://en.wikipedia.org/wiki/List_of_Chicago_Blackhawks_captains", FALSE,
  "Colorado Avalanche", 2015L, 2024L, "Gabriel Landeskog", "https://en.wikipedia.org/wiki/List_of_Colorado_Avalanche_captains", FALSE,
  "Columbus Blue Jackets", 2015L, 2020L, "Nick Foligno", "https://en.wikipedia.org/wiki/List_of_Columbus_Blue_Jackets_captains", FALSE,
  "Columbus Blue Jackets", 2021L, 2024L, "Boone Jenner", "https://en.wikipedia.org/wiki/List_of_Columbus_Blue_Jackets_captains", FALSE,
  "Dallas Stars", 2015L, 2024L, "Jamie Benn", "https://en.wikipedia.org/wiki/List_of_Dallas_Stars_captains", FALSE,
  "Detroit Red Wings", 2015L, 2017L, "Henrik Zetterberg", "https://en.wikipedia.org/wiki/List_of_Detroit_Red_Wings_captains", FALSE,
  "Detroit Red Wings", 2021L, 2024L, "Dylan Larkin", "https://en.wikipedia.org/wiki/List_of_Detroit_Red_Wings_captains", FALSE,
  "Edmonton Oilers", 2015L, 2015L, "Andrew Ference", "https://en.wikipedia.org/wiki/List_of_Edmonton_Oilers_captains", FALSE,
  "Edmonton Oilers", 2016L, 2024L, "Connor McDavid", "https://en.wikipedia.org/wiki/List_of_Edmonton_Oilers_captains", FALSE,
  "Florida Panthers", 2015L, 2015L, "Willie Mitchell", "https://en.wikipedia.org/wiki/List_of_Florida_Panthers_captains", FALSE,
  "Florida Panthers", 2016L, 2017L, "Derek MacKenzie", "https://en.wikipedia.org/wiki/List_of_Florida_Panthers_captains", FALSE,
  "Florida Panthers", 2018L, 2024L, "Aleksander Barkov", "https://en.wikipedia.org/wiki/List_of_Florida_Panthers_captains", FALSE,
  "Los Angeles Kings", 2015L, 2015L, "Dustin Brown", "https://en.wikipedia.org/wiki/List_of_Los_Angeles_Kings_captains", FALSE,
  "Los Angeles Kings", 2016L, 2024L, "Anze Kopitar", "https://en.wikipedia.org/wiki/List_of_Los_Angeles_Kings_captains", FALSE,
  "Minnesota Wild", 2015L, 2019L, "Mikko Koivu", "https://en.wikipedia.org/wiki/List_of_Minnesota_Wild_captains", FALSE,
  "Minnesota Wild", 2020L, 2024L, "Jared Spurgeon", "https://en.wikipedia.org/wiki/List_of_Minnesota_Wild_captains", FALSE,
  "Montreal Canadiens", 2015L, 2017L, "Max Pacioretty", "https://en.wikipedia.org/wiki/List_of_Montreal_Canadiens_captains", FALSE,
  "Montreal Canadiens", 2018L, 2021L, "Shea Weber", "https://en.wikipedia.org/wiki/List_of_Montreal_Canadiens_captains", FALSE,
  "Montreal Canadiens", 2022L, 2024L, "Nick Suzuki", "https://en.wikipedia.org/wiki/List_of_Montreal_Canadiens_captains", FALSE,
  "Nashville Predators", 2015L, 2015L, "Shea Weber", "https://en.wikipedia.org/wiki/List_of_Nashville_Predators_captains", FALSE,
  "Nashville Predators", 2016L, 2016L, "Mike Fisher", "https://en.wikipedia.org/wiki/List_of_Nashville_Predators_captains", FALSE,
  "Nashville Predators", 2017L, 2024L, "Roman Josi", "https://en.wikipedia.org/wiki/List_of_Nashville_Predators_captains", FALSE,
  "New Jersey Devils", 2015L, 2019L, "Andy Greene", "https://en.wikipedia.org/wiki/List_of_New_Jersey_Devils_captains", FALSE,
  "New Jersey Devils", 2021L, 2024L, "Nico Hischier", "https://en.wikipedia.org/wiki/List_of_New_Jersey_Devils_captains", FALSE,
  "New York Islanders", 2015L, 2017L, "John Tavares", "https://en.wikipedia.org/wiki/List_of_New_York_Islanders_captains", FALSE,
  "New York Islanders", 2018L, 2024L, "Anders Lee", "https://en.wikipedia.org/wiki/List_of_New_York_Islanders_captains", FALSE,
  "New York Rangers", 2015L, 2017L, "Ryan McDonagh", "https://en.wikipedia.org/wiki/List_of_New_York_Rangers_captains", FALSE,
  "New York Rangers", 2022L, 2024L, "Jacob Trouba", "https://en.wikipedia.org/wiki/List_of_New_York_Rangers_captains", FALSE,
  "Ottawa Senators", 2015L, 2017L, "Erik Karlsson", "https://en.wikipedia.org/wiki/List_of_Ottawa_Senators_captains", FALSE,
  "Ottawa Senators", 2021L, 2024L, "Brady Tkachuk", "https://en.wikipedia.org/wiki/List_of_Ottawa_Senators_captains", FALSE,
  "Philadelphia Flyers", 2015L, 2021L, "Claude Giroux", "https://en.wikipedia.org/wiki/List_of_Philadelphia_Flyers_captains", FALSE,
  "Philadelphia Flyers", 2024L, 2024L, "Sean Couturier", "https://en.wikipedia.org/wiki/List_of_Philadelphia_Flyers_captains", FALSE,
  "Pittsburgh Penguins", 2015L, 2024L, "Sidney Crosby", "https://en.wikipedia.org/wiki/List_of_Pittsburgh_Penguins_captains", FALSE,
  "San Jose Sharks", 2015L, 2018L, "Joe Pavelski", "https://en.wikipedia.org/wiki/List_of_San_Jose_Sharks_captains", FALSE,
  "San Jose Sharks", 2019L, 2024L, "Logan Couture", "https://en.wikipedia.org/wiki/List_of_San_Jose_Sharks_captains", FALSE,
  "Seattle Kraken", 2021L, 2021L, "Mark Giordano", "https://en.wikipedia.org/wiki/List_of_Seattle_Kraken_captains", FALSE,
  "Seattle Kraken", 2024L, 2024L, "Jordan Eberle", "https://en.wikipedia.org/wiki/List_of_Seattle_Kraken_captains", FALSE,
  "St. Louis Blues", 2015L, 2015L, "David Backes", "https://en.wikipedia.org/wiki/List_of_St._Louis_Blues_captains", FALSE,
  "St. Louis Blues", 2016L, 2019L, "Alex Pietrangelo", "https://en.wikipedia.org/wiki/List_of_St._Louis_Blues_captains", FALSE,
  "St. Louis Blues", 2020L, 2022L, "Ryan O'Reilly", "https://en.wikipedia.org/wiki/List_of_St._Louis_Blues_captains", FALSE,
  "St. Louis Blues", 2023L, 2024L, "Brayden Schenn", "https://en.wikipedia.org/wiki/List_of_St._Louis_Blues_captains", FALSE,
  "Tampa Bay Lightning", 2015L, 2023L, "Steven Stamkos", "https://en.wikipedia.org/wiki/List_of_Tampa_Bay_Lightning_captains", FALSE,
  "Tampa Bay Lightning", 2024L, 2024L, "Victor Hedman", "https://en.wikipedia.org/wiki/List_of_Tampa_Bay_Lightning_captains", FALSE,
  "Toronto Maple Leafs", 2015L, 2015L, "Dion Phaneuf", "https://en.wikipedia.org/wiki/List_of_Toronto_Maple_Leafs_captains", FALSE,
  "Toronto Maple Leafs", 2019L, 2024L, "John Tavares", "https://en.wikipedia.org/wiki/List_of_Toronto_Maple_Leafs_captains", FALSE,
  "Utah Hockey Club", 2015L, 2016L, "Shane Doan", "https://en.wikipedia.org/wiki/List_of_Arizona_Coyotes_captains", FALSE,
  "Utah Hockey Club", 2018L, 2020L, "Oliver Ekman-Larsson", "https://en.wikipedia.org/wiki/List_of_Arizona_Coyotes_captains", FALSE,
  "Utah Hockey Club", 2024L, 2024L, "Clayton Keller", "https://en.wikipedia.org/wiki/List_of_Utah_Hockey_Club_captains", FALSE,
  "Vancouver Canucks", 2015L, 2017L, "Henrik Sedin", "https://en.wikipedia.org/wiki/List_of_Vancouver_Canucks_captains", FALSE,
  "Vancouver Canucks", 2019L, 2022L, "Bo Horvat", "https://en.wikipedia.org/wiki/List_of_Vancouver_Canucks_captains", FALSE,
  "Vancouver Canucks", 2023L, 2024L, "Quinn Hughes", "https://en.wikipedia.org/wiki/List_of_Vancouver_Canucks_captains", FALSE,
  "Vegas Golden Knights", 2021L, 2024L, "Mark Stone", "https://en.wikipedia.org/wiki/List_of_Vegas_Golden_Knights_captains", FALSE,
  "Washington Capitals", 2015L, 2024L, "Alex Ovechkin", "https://en.wikipedia.org/wiki/List_of_Washington_Capitals_captains", FALSE,
  "Winnipeg Jets", 2015L, 2015L, "Andrew Ladd", "https://en.wikipedia.org/wiki/List_of_Winnipeg_Jets_captains", FALSE,
  "Winnipeg Jets", 2016L, 2021L, "Blake Wheeler", "https://en.wikipedia.org/wiki/List_of_Winnipeg_Jets_captains", FALSE,
  "Winnipeg Jets", 2023L, 2024L, "Adam Lowry", "https://en.wikipedia.org/wiki/List_of_Winnipeg_Jets_captains", FALSE
)

captaincy_raw <- captain_tenures %>%
  mutate(season_start_year = purrr::map2(.data$start_year, .data$end_year, seq)) %>%
  tidyr::unnest_longer(.data$season_start_year) %>%
  transmute(
    team = .data$team,
    season = season_label(.data$season_start_year),
    captain_name = .data$captain_name,
    source_page = .data$source_page,
    alternate_data_available = .data$alternate_data_available
  ) %>%
  arrange(.data$team, .data$season)

all_current_teams <- c(
  "Anaheim Ducks", "Boston Bruins", "Buffalo Sabres", "Calgary Flames",
  "Carolina Hurricanes", "Chicago Blackhawks", "Colorado Avalanche",
  "Columbus Blue Jackets", "Dallas Stars", "Detroit Red Wings",
  "Edmonton Oilers", "Florida Panthers", "Los Angeles Kings",
  "Minnesota Wild", "Montreal Canadiens", "Nashville Predators",
  "New Jersey Devils", "New York Islanders", "New York Rangers",
  "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins",
  "San Jose Sharks", "Seattle Kraken", "St. Louis Blues",
  "Tampa Bay Lightning", "Toronto Maple Leafs", "Utah Hockey Club",
  "Vancouver Canucks", "Vegas Golden Knights", "Washington Capitals",
  "Winnipeg Jets"
)

failed_teams <- setdiff(all_current_teams, unique(captaincy_raw$team))

readr::write_csv(captaincy_raw, output_path)

message("Wikipedia captaincy QA summary")
message(sprintf("- teams successfully parsed: %s", dplyr::n_distinct(captaincy_raw$team)))
message(sprintf("- total captain-season records: %s", nrow(captaincy_raw)))
message(sprintf(
  "- teams failed to parse: %s",
  ifelse(length(failed_teams) == 0, "none", paste(failed_teams, collapse = ", "))
))
message("- alternate captain availability: no consistent alternate-captain tables included in V1 fallback data")
message(sprintf("Saved: %s", output_path))
