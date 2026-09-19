# =============================================================================
# run_simulation.R  --  EPL Match Prediction Engine & Monte Carlo Simulation
# -----------------------------------------------------------------------------
# Usage:
#   Rscript run_simulation.R                 # real data (downloads if missing)
#   USE_SYNTHETIC=1 Rscript run_simulation.R # synthetic data, no download
#
# Packages: dplyr, tidyr, readr, tibble, lubridate, ggplot2, scales
# Optional: ggridges (ridge plot), future.apply (parallel MC), testthat (tests)
# Report:   Quarto CLI (https://quarto.org) + knitr, kableExtra. Skipped if absent.
# =============================================================================

config <- list(
  seasons        = c("2425", "2526", "2627"),   # history used to fit ratings
  current_season = "2627",                      # season being simulated
  fixtures_file  = NULL,   # optional fixturedownload.com CSV; NULL = derive fixtures
  n_sims         = 10000,
  xi             = 0.0018,  # time-decay per day (0 = all matches weighted equally)
  ridge          = 1.0,     # global shrinkage; backtest: keep at 1 (10 hurts)
  prior_matches  = 0,       # pseudo-match prior for low-data teams; set after backtest round 2
  max_goals      = 10,
  param_draws    = 500,     # rating vectors sampled from the posterior (1 = point estimate);
                            # 500 draws x 20 seasons each keeps draw-noise on the means < 1 pt
  seed           = 2026,
  use_synthetic  = nzchar(Sys.getenv("USE_SYNTHETIC")),
  output_dir     = "output",
  render_report  = !nzchar(Sys.getenv("SKIP_REPORT"))   # SKIP_REPORT=1 to skip
)

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
library(tidyr)

# --- 1. Data ------------------------------------------------------------------
if (config$use_synthetic) {
  message("Using SYNTHETIC data")
  syn     <- make_synthetic_data()
  results <- syn$results
  current <- filter(results, season == "current")
} else {
  paths   <- download_football_data(config$seasons, dir = "data/raw")
  results <- load_results(paths)
  current <- filter(results, season == config$current_season)
}

base_table <- current_table(current)
fixtures   <- if (is.null(config$fixtures_file)) {
  derive_remaining_fixtures(current, teams = base_table$team)
} else {
  load_fixturedownload(config$fixtures_file, known_teams = base_table$team)
}

# --- 2. Team strength ---------------------------------------------------------
fit <- fit_dixon_coles(results, xi = config$xi, ridge = config$ridge,
                       prior_matches = config$prior_matches)
print(fit)
print(strength_table(fit, teams = base_table$team), n = 20)
cat("\nRating uncertainty (least-measured teams first):\n")
print(rating_uncertainty(fit) %>% filter(team %in% base_table$team) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))), n = 5)

# --- 3 & 4. Match engine + Monte Carlo -----------------------------------------
sim <- run_monte_carlo(fit, fixtures, base_table,
                       n_sims = config$n_sims, seed = config$seed,
                       max_goals = config$max_goals, param_draws = config$param_draws)

# --- 5. Summaries -------------------------------------------------------------
summary_tbl <- summarise_simulation(sim)
cat("\n=== Season forecast ===\n")
print(format_summary(summary_tbl) %>% select(-eff_matches), n = 20, width = 120)
if (any(summary_tbl$low_data)) {
  cat(sprintf("\nNote: %s rated on < 10 effective matches; their tail probabilities (title, relegation) are wide and will tighten as the season progresses.\n",
              paste(summary_tbl$team[summary_tbl$low_data], collapse = ", ")))
}

cat("\n=== Next fixtures (sample) ===\n")
print(sim$fixtures %>% select(home, away, lambda, mu, p_home, p_draw, p_away) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>% head(10))

# Upcoming fixtures (next ~week, dated) for the report; real data only
upcoming <- NULL
if (!config$use_synthetic) {
  upcoming <- tryCatch({
    fx_path <- download_upcoming_fixtures("data/raw")
    upcoming_fixture_probs(fit, load_upcoming_fixtures(fx_path, teams = base_table$team),
                           max_goals = config$max_goals)
  }, error = function(e) { message("Upcoming fixtures unavailable: ", conditionMessage(e)); NULL })
}

dir.create(config$output_dir, showWarnings = FALSE)
if (!is.null(upcoming) && nrow(upcoming)) {
  readr::write_csv(upcoming, file.path(config$output_dir, "upcoming_fixtures.csv"))
} else {
  unlink(file.path(config$output_dir, "upcoming_fixtures.csv"))   # don't show a stale one
}
readr::write_csv(summary_tbl, file.path(config$output_dir, "season_forecast.csv"))
readr::write_csv(position_probabilities(sim), file.path(config$output_dir, "position_probabilities.csv"))
readr::write_csv(sim$fixtures, file.path(config$output_dir, "fixture_probabilities.csv"))
saveRDS(list(fit = fit, sim = sim, config = config), file.path(config$output_dir, "simulation.rds"))

# --- 6. Plots -----------------------------------------------------------------
figs <- save_all_plots(sim, summary_tbl, dir = config$output_dir)
message("Wrote: ", paste(basename(figs), collapse = ", "), " to ", config$output_dir)

# --- 7. Report ----------------------------------------------------------------
if (config$render_report) {
  render_report(matchweek = matchweek_number(base_table), run_date = Sys.Date(),
                reports_dir = file.path(config$output_dir, "reports"),
                rds = file.path(config$output_dir, "simulation.rds"),
                upcoming_csv = file.path(config$output_dir, "upcoming_fixtures.csv"),
                output_dir = config$output_dir)
}
