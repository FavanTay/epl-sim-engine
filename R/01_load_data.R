# =============================================================================
# 01_load_data.R  --  Data acquisition and preparation
# -----------------------------------------------------------------------------
# Canonical results schema used by every downstream module:
#   date   : Date
#   home   : chr   home team name
#   away   : chr   away team name
#   hg     : int   full-time home goals
#   ag     : int   full-time away goals
#   season : chr   e.g. "2627"
#
# Canonical fixtures schema (remaining, unplayed matches):
#   date, home, away        (date may be NA if derived rather than loaded)
#
# Primary source: football-data.co.uk  (free CSV per season, stable schema).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(lubridate)
})

FD_BASE_URL <- "https://www.football-data.co.uk/mmz4281"

# --- Download -----------------------------------------------------------------

#' Download E0.csv (Premier League) for one or more seasons.
#' @param seasons character vector of 4-digit season codes, e.g. "2627" = 2026/27
#' @return character vector of local file paths
download_football_data <- function(seasons = c("2425", "2526", "2627"),
                                   dir = "data/raw", overwrite = FALSE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  vapply(seasons, function(s) {
    dest <- file.path(dir, paste0("E0_", s, ".csv"))
    if (overwrite || !file.exists(dest)) {
      url <- sprintf("%s/%s/E0.csv", FD_BASE_URL, s)
      message("Downloading ", url)
      download.file(url, dest, quiet = TRUE, mode = "wb")
    }
    dest
  }, character(1), USE.NAMES = FALSE)
}

# --- Load results -------------------------------------------------------------

#' Read one or more football-data.co.uk CSVs into the canonical results schema.
#' Rows without a full-time score (not yet played / malformed) are dropped.
load_results <- function(paths) {
  purrr_map <- function(p) {
    raw <- read_csv(p, col_types = cols(.default = col_character()),
                    show_col_types = FALSE, progress = FALSE)
    season <- sub("^E0_(\\d{4})\\.csv$", "\\1", basename(p))
    raw %>%
      transmute(
        date   = dmy(Date),                    # handles dd/mm/yy and dd/mm/yyyy
        home   = trimws(HomeTeam),
        away   = trimws(AwayTeam),
        hg     = as.integer(FTHG),
        ag     = as.integer(FTAG),
        season = season
      ) %>%
      filter(!is.na(date), !is.na(hg), !is.na(ag), home != "", away != "")
  }
  bind_rows(lapply(paths, purrr_map)) %>% arrange(date)
}

# --- Remaining fixtures -------------------------------------------------------

#' Derive the unplayed fixtures of the current season: every ordered pair
#' (home, away) of the 20 teams minus the pairings already played.
#' No external fixture file needed. Dates are NA (irrelevant to the simulation
#' because ratings are frozen at simulation time).
derive_remaining_fixtures <- function(current_results, teams = NULL) {
  if (is.null(teams)) teams <- sort(unique(c(current_results$home, current_results$away)))
  if (length(teams) != 20) {
    warning("Expected 20 teams in current season, found ", length(teams),
            ". Check team names / that every team has played.")
  }
  all_pairs <- expand.grid(home = teams, away = teams, stringsAsFactors = FALSE) %>%
    filter(home != away) %>% as_tibble()
  remaining <- anti_join(all_pairs, current_results, by = c("home", "away")) %>%
    mutate(date = as.Date(NA)) %>%
    select(date, home, away)
  message(nrow(current_results), " played, ", nrow(remaining), " remaining (",
          nrow(current_results) + nrow(remaining), " total)")
  remaining
}

#' Optional: load the full-season schedule from fixturedownload.com
#' (columns: "Match Number","Round Number","Date","Location","Home Team",
#'  "Away Team","Result") and keep only unplayed rows. Team names there differ
#' from football-data.co.uk, so a mapping is applied and validated.
load_fixturedownload <- function(path, known_teams, name_map = fixturedownload_name_map()) {
  raw <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  fx <- raw %>%
    transmute(date = as.Date(dmy_hm(Date)),
              home = recode(`Home Team`, !!!name_map),
              away = recode(`Away Team`, !!!name_map),
              result = Result) %>%
    filter(is.na(result) | result == "") %>%
    select(date, home, away)
  unknown <- setdiff(unique(c(fx$home, fx$away)), known_teams)
  if (length(unknown) > 0) {
    stop("Unmapped team names in fixture file: ", paste(unknown, collapse = ", "),
         "\nAdd them to fixturedownload_name_map().")
  }
  fx
}

#' fixturedownload.com  ->  football-data.co.uk naming.  Extend as needed.
fixturedownload_name_map <- function() c(
  "Man Utd"        = "Man United",
  "Man City"       = "Man City",
  "Spurs"          = "Tottenham",
  "Nott'm Forest"  = "Nott'm Forest",
  "Newcastle"      = "Newcastle",
  "Wolves"         = "Wolves",
  "Brighton"       = "Brighton",
  "Sheffield Utd"  = "Sheffield United"
)

# --- Upcoming fixtures (next ~7 days, with dates) -----------------------------

#' football-data.co.uk publishes the coming week's fixtures (all leagues) in
#' one file. Used for the report's "upcoming fixtures" table.
download_upcoming_fixtures <- function(dir = "data/raw", overwrite = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, "fixtures.csv")
  if (overwrite || !file.exists(dest)) {
    download.file("https://www.football-data.co.uk/fixtures.csv", dest, quiet = TRUE, mode = "wb")
  }
  dest
}

#' Parse fixtures.csv into (date, time, home, away) for the Premier League,
#' keeping bookmaker H/D/A odds if present (columns B365H/B365D/B365A) so the
#' report can show the market alongside the model.
load_upcoming_fixtures <- function(path, teams = NULL) {
  raw <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  fx <- raw %>%
    filter(Div == "E0") %>%
    transmute(date = dmy(Date),
              time = if ("Time" %in% names(raw)) Time else NA_character_,
              home = trimws(HomeTeam), away = trimws(AwayTeam),
              odds_h = if ("B365H" %in% names(raw)) as.numeric(B365H) else NA_real_,
              odds_d = if ("B365D" %in% names(raw)) as.numeric(B365D) else NA_real_,
              odds_a = if ("B365A" %in% names(raw)) as.numeric(B365A) else NA_real_) %>%
    filter(!is.na(date)) %>%
    arrange(date, time)
  if (!is.null(teams)) {
    unknown <- setdiff(unique(c(fx$home, fx$away)), teams)
    if (length(unknown)) warning("Upcoming fixtures with unknown teams dropped: ",
                                 paste(unknown, collapse = ", "))
    fx <- fx %>% filter(home %in% teams, away %in% teams)
  }
  fx
}

# --- League table -------------------------------------------------------------

#' Build the current league table (P, W, D, L, GF, GA, GD, Pts) from played
#' matches. Returned in canonical order: Pts -> GD -> GF -> name.
current_table <- function(current_results, teams = NULL) {
  long <- bind_rows(
    current_results %>% transmute(team = home, gf = hg, ga = ag),
    current_results %>% transmute(team = away, gf = ag, ga = hg)
  )
  tbl <- long %>%
    group_by(team) %>%
    summarise(
      played = n(),
      won    = sum(gf > ga), drawn = sum(gf == ga), lost = sum(gf < ga),
      gf = sum(gf), ga = sum(ga), .groups = "drop"
    ) %>%
    mutate(gd = gf - ga, pts = 3L * won + drawn)
  if (!is.null(teams)) {
    missing <- setdiff(teams, tbl$team)
    if (length(missing)) {
      tbl <- bind_rows(tbl, tibble(team = missing, played = 0L, won = 0L, drawn = 0L,
                                   lost = 0L, gf = 0L, ga = 0L, gd = 0L, pts = 0L))
    }
  }
  tbl %>% arrange(desc(pts), desc(gd), desc(gf), team)
}

# --- Synthetic data (for development & tests when no CSVs are present) ---------

#' Generate a fake league history with known "true" strengths so the pipeline
#' can be run end-to-end without downloading anything.
#' @return list(results = tibble, truth = tibble of true parameters)
make_synthetic_data <- function(n_past_seasons = 2, rounds_played = 5,
                                n_teams = 20, seed = 1) {
  set.seed(seed)
  teams   <- sprintf("Team %02d", seq_len(n_teams))
  attack  <- rnorm(n_teams, 0, 0.25)
  defence <- rnorm(n_teams, 0, 0.20)
  home_adv <- 0.25

  play_pairs <- function(pairs, season, start) {
    lam <- exp(attack[match(pairs$home, teams)] + defence[match(pairs$away, teams)] + home_adv)
    mu  <- exp(attack[match(pairs$away, teams)] + defence[match(pairs$home, teams)])
    pairs %>% mutate(
      date   = start + sample(0:270, n(), replace = TRUE),
      hg     = rpois(n(), lam), ag = rpois(n(), mu),
      season = season
    ) %>% select(date, home, away, hg, ag, season)
  }
  all_pairs <- expand.grid(home = teams, away = teams, stringsAsFactors = FALSE) %>%
    filter(home != away) %>% as_tibble()

  seasons <- list()
  for (k in seq_len(n_past_seasons)) {
    start <- as.Date("2026-08-15") - years(n_past_seasons - k + 1)
    seasons[[k]] <- play_pairs(all_pairs, sprintf("past%d", k), start)
  }
  # Current season: a random subset of rounds_played * (n_teams/2) matches
  current <- play_pairs(all_pairs %>% slice_sample(n = rounds_played * n_teams / 2),
                        "current", as.Date("2026-08-15")) %>%
    mutate(date = as.Date("2026-08-15") + sample(0:30, n(), replace = TRUE))

  list(
    results = bind_rows(seasons, current) %>% arrange(date),
    truth   = tibble(team = teams, attack = attack, defence = defence),
    home_adv = home_adv
  )
}
