# =============================================================================
# run_backtest.R  --  Stage 5: calibration & tuning
# -----------------------------------------------------------------------------
# Usage:
#   Rscript run_backtest.R                    # weekly origins, ~10-15 min
#   BACKTEST_STEP=14 Rscript run_backtest.R   # fortnightly origins, ~half the time
#   BACKTEST_PARALLEL=1 Rscript run_backtest.R  (needs install.packages("future.apply"))
#
# Training data must start at least one season before the first test season,
# otherwise the first origins of 2024/25 have nothing to fit on.
# =============================================================================

bt_config <- list(
  seasons      = c("2324", "2425", "2526"),   # downloaded; 2324 is training-only
  test_seasons = c("2425", "2526"),
  # Round 1 (done): xi in {0..0.005} x ridge in {1,10} -> xi flat, ridge=10 hurts.
  # Round 2: hold xi/ridge at production values, tune the promoted-team prior.
  xi_grid      = c(0.0018),
  ridge_grid   = c(1, 3, 5),
  prior_grid   = c(0),             # 0 = current production value
  step_days    = as.integer(Sys.getenv("BACKTEST_STEP", "7")),
  parallel     = nzchar(Sys.getenv("BACKTEST_PARALLEL")),
  output_dir   = "output"
)

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

paths   <- download_football_data(bt_config$seasons, dir = "data/raw")
results <- load_results_with_odds(paths)
cat(sprintf("%d matches loaded; odds coverage: %.1f%% (sources: %s)\n",
            nrow(results), 100 * mean(!is.na(results$odds_h)),
            paste(names(table(results$odds_source)), collapse = ", ")))

bt <- run_backtest(results,
                   test_seasons = bt_config$test_seasons,
                   xi_grid      = bt_config$xi_grid,
                   ridge_grid   = bt_config$ridge_grid,
                   prior_grid   = bt_config$prior_grid,
                   step_days    = bt_config$step_days,
                   parallel     = bt_config$parallel)

cat("\n=== Backtest summary (lower is better) ===\n")
print(bt$summary %>% mutate(across(c(log_loss, brier, log_loss_se, any_of("vs_bookmaker")), ~ round(.x, 4))),
      n = 50, width = 120)

cat("\n=== By season ===\n")
print(summarise_backtest(bt$matches, by = "season") %>%
        mutate(across(c(log_loss, brier, log_loss_se, any_of("vs_bookmaker")), ~ round(.x, 4))),
      n = 50, width = 120)

best <- bt$summary %>% filter(!is.na(xi)) %>% slice_min(log_loss, n = 1)
cat(sprintf("\nBest Dixon-Coles setting: xi = %.4f, ridge = %g, prior_matches = %g (log-loss %.4f)\n",
            best$xi, best$ridge, best$prior, best$log_loss))

# Where the prior should matter: early-season matches involving newly promoted
# sides (teams absent from the previous season's data).
prev_teams <- function(s) unique(c(results$home[results$season == s], results$away[results$season == s]))
seasons_sorted <- sort(unique(results$season))
promoted <- unlist(lapply(seq_along(seasons_sorted)[-1], function(i)
  setdiff(prev_teams(seasons_sorted[i]), prev_teams(seasons_sorted[i - 1]))))
cat("\n=== Early-season (Aug-Oct) matches involving promoted sides:",
    paste(unique(promoted), collapse = ", "), "===\n")
print(bt$matches %>%
        filter(lubridate::month(date) %in% 8:10, home %in% promoted | away %in% promoted) %>%
        summarise_backtest() %>%
        mutate(across(c(log_loss, brier, log_loss_se, any_of("vs_bookmaker")), ~ round(.x, 4))),
      n = 50, width = 120)

dir.create(bt_config$output_dir, showWarnings = FALSE)
write_csv(bt$summary, file.path(bt_config$output_dir, "backtest_summary.csv"))
write_csv(bt$matches, file.path(bt_config$output_dir, "backtest_matches.csv"))
ggplot2::ggsave(file.path(bt_config$output_dir, "backtest_logloss.png"), plot_backtest(bt),
                width = 7, height = 4.5, dpi = 150)
message("Wrote backtest_summary.csv, backtest_matches.csv, backtest_logloss.png to ", bt_config$output_dir)
