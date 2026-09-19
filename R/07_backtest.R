# =============================================================================
# 07_backtest.R  --  Rolling-origin backtest & calibration
# -----------------------------------------------------------------------------
# For each origin date t (weekly by default) and each (xi, ridge) combination:
#   1. fit_dixon_coles() on every match with date < t          (no look-ahead)
#   2. forecast the matches in [t, t + step_days) of the test seasons
#   3. score each forecast: log-loss and Brier on the H/D/A outcome
# The same matches are scored against de-vigged closing odds (the market's
# forecast) and a naive base-rate forecast, so every row of the summary table
# is directly comparable.
#
# fit_dixon_coles() is called exactly as in production; its interface is
# untouched. This file defines functions only -- run_backtest.R drives it, so
# sourcing R/*.R in run_simulation.R does not trigger a backtest.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(lubridate)
})

# --- Odds --------------------------------------------------------------------

# football-data.co.uk column triples, best first. "C" suffix = closing odds
# (available from 2019/20). Falls back to pre-match odds if closing are absent.
ODDS_PRIORITY <- list(
  pinnacle_closing = c("PSCH",   "PSCD",   "PSCA"),
  average_closing  = c("AvgCH",  "AvgCD",  "AvgCA"),
  bet365_closing   = c("B365CH", "B365CD", "B365CA"),
  pinnacle_open    = c("PSH",    "PSD",    "PSA"),
  bet365_open      = c("B365H",  "B365D",  "B365A")
)

#' Like load_results() but keeps one set of H/D/A odds per match.
#' Adds columns odds_h, odds_d, odds_a, odds_source (NA where unavailable).
load_results_with_odds <- function(paths, priority = ODDS_PRIORITY) {
  read_one <- function(p) {
    raw <- read_csv(p, col_types = cols(.default = col_character()),
                    show_col_types = FALSE, progress = FALSE)
    season <- sub("^E0_(\\d{4})\\.csv$", "\\1", basename(p))
    base <- raw %>% transmute(
      date = dmy(Date), home = trimws(HomeTeam), away = trimws(AwayTeam),
      hg = as.integer(FTHG), ag = as.integer(FTAG), season = season,
      odds_h = NA_real_, odds_d = NA_real_, odds_a = NA_real_, odds_source = NA_character_
    )
    # Fill odds row-by-row from the best available source
    for (src in names(priority)) {
      cols <- priority[[src]]
      if (!all(cols %in% names(raw))) next
      o <- suppressWarnings(sapply(raw[cols], as.numeric))
      ok <- is.na(base$odds_h) & complete.cases(o) & rowSums(o > 1) == 3
      base$odds_h[ok] <- o[ok, 1]; base$odds_d[ok] <- o[ok, 2]; base$odds_a[ok] <- o[ok, 3]
      base$odds_source[ok] <- src
    }
    base %>% filter(!is.na(date), !is.na(hg), !is.na(ag))
  }
  bind_rows(lapply(paths, read_one)) %>% arrange(date)
}

#' Remove the bookmaker margin by proportional normalisation of implied
#' probabilities. Returns a 3-column matrix (p_home, p_draw, p_away).
devig <- function(odds_h, odds_d, odds_a) {
  inv <- cbind(1 / odds_h, 1 / odds_d, 1 / odds_a)
  out <- inv / rowSums(inv)
  colnames(out) <- c("p_home", "p_draw", "p_away")
  out
}

# --- Scoring -----------------------------------------------------------------

#' Per-match log-loss and Brier score for H/D/A probability forecasts.
score_forecasts <- function(p_home, p_draw, p_away, hg, ag) {
  P <- cbind(p_home, p_draw, p_away)
  Y <- cbind(hg > ag, hg == ag, hg < ag) * 1
  tibble(
    log_loss = -log(pmax(rowSums(P * Y), 1e-12)),
    brier    = rowSums((P - Y)^2)
  )
}

# --- Backtest ----------------------------------------------------------------

#' Rolling-origin backtest over a grid of (xi, ridge).
#'
#' @param results       tibble from load_results_with_odds() (odds optional);
#'                      must include seasons BEFORE the first test season so
#'                      early-season forecasts have training data
#' @param test_seasons  season codes to score, e.g. c("2425", "2526")
#' @param xi_grid, ridge_grid, prior_grid  parameter values to evaluate (full
#'                      cross); prior_grid is passed as prior_matches
#' @param step_days     spacing of origins and width of each forecast window
#' @param require_odds  if TRUE, only matches with odds are scored so model
#'                      and bookmaker rows share the same match set
#' @param parallel      use future.apply over origins (install future.apply)
#' @return list(matches = per-match forecasts & scores for every model,
#'              summary = one row per model, grid = the grid used)
run_backtest <- function(results, test_seasons, xi_grid = c(0, 0.001, 0.0018, 0.003, 0.005),
                         ridge_grid = 1, prior_grid = 0, step_days = 7, max_goals = 10,
                         require_odds = "odds_h" %in% names(results),
                         parallel = FALSE, verbose = TRUE) {
  stopifnot(all(test_seasons %in% results$season))
  if (!"odds_h" %in% names(results)) require_odds <- FALSE

  test_dates <- results$date[results$season %in% test_seasons]
  origins <- seq(min(test_dates), max(test_dates), by = step_days)
  grid    <- expand.grid(xi = xi_grid, ridge = ridge_grid, prior = prior_grid) %>% as_tibble()
  if (verbose) message(sprintf("Backtest: %d origins x %d parameter sets = %d fits",
                               length(origins), nrow(grid), length(origins) * nrow(grid)))

  one_origin <- function(t) {
    train <- results %>% filter(date < t)
    test  <- results %>% filter(date >= t, date < t + step_days, season %in% test_seasons)
    if (require_odds) test <- test %>% filter(!is.na(odds_h))
    if (nrow(test) == 0 || nrow(train) < 50) return(NULL)

    known <- unique(c(train$home, train$away))
    test  <- test %>% filter(home %in% known, away %in% known)   # can't rate unseen teams
    if (nrow(test) == 0) return(NULL)

    fx <- test %>% select(date, home, away, hg, ag, season)

    # --- Dixon-Coles for every grid point ---
    model_rows <- lapply(seq_len(nrow(grid)), function(g) {
      fit <- suppressWarnings(fit_dixon_coles(train, xi = grid$xi[g], ridge = grid$ridge[g],
                                              prior_matches = grid$prior[g],
                                              ref_date = t, hessian = FALSE, verbose = FALSE))
      mp  <- match_probs(fit, fx %>% select(home, away), max_goals)
      bind_cols(fx, mp %>% select(p_home, p_draw, p_away)) %>%
        mutate(model = sprintf("DC xi=%.4f ridge=%g prior=%g", grid$xi[g], grid$ridge[g], grid$prior[g]),
               xi = grid$xi[g], ridge = grid$ridge[g], prior = grid$prior[g])
    })

    # --- Naive base rates from the training window ---
    base <- c(mean(train$hg > train$ag), mean(train$hg == train$ag), mean(train$hg < train$ag))
    naive <- fx %>% mutate(p_home = base[1], p_draw = base[2], p_away = base[3],
                           model = "Naive base rates", xi = NA_real_, ridge = NA_real_, prior = NA_real_)

    out <- bind_rows(model_rows, naive)

    # --- Bookmaker (de-vigged closing odds) ---
    if ("odds_h" %in% names(test) && any(!is.na(test$odds_h))) {
      has <- !is.na(test$odds_h)
      bk  <- fx[has, ] %>%
        bind_cols(as_tibble(devig(test$odds_h[has], test$odds_d[has], test$odds_a[has]))) %>%
        mutate(model = "Bookmaker (de-vigged closing)", xi = NA_real_, ridge = NA_real_, prior = NA_real_)
      out <- bind_rows(out, bk)
    }

    out %>% mutate(origin = t, .before = 1)
  }

  t0 <- Sys.time()
  parts <- if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) stop("Install future.apply")
    future::plan(future::multisession)
    on.exit(future::plan(future::sequential), add = TRUE)
    future.apply::future_lapply(origins, one_origin, future.seed = TRUE)
  } else {
    lapply(seq_along(origins), function(i) {
      if (verbose && i %% 10 == 0) message(sprintf("  origin %d/%d (%s)", i, length(origins), origins[i]))
      one_origin(origins[i])
    })
  }
  matches <- bind_rows(parts)
  matches <- bind_cols(matches, score_forecasts(matches$p_home, matches$p_draw,
                                                matches$p_away, matches$hg, matches$ag))
  if (verbose) message(sprintf("Done in %.1f min", as.numeric(Sys.time() - t0, units = "mins")))

  list(matches = matches, summary = summarise_backtest(matches), grid = grid)
}

#' One row per model: match count, mean log-loss, mean Brier, and the
#' log-loss gap to the bookmaker (positive = worse than the market).
summarise_backtest <- function(matches, by = NULL) {
  s <- matches %>%
    group_by(model, xi, ridge, prior, across(all_of(by))) %>%
    summarise(n = n(), log_loss_se = sd(log_loss) / sqrt(n()),
              log_loss = mean(log_loss), brier = mean(brier), .groups = "drop") %>%
    relocate(log_loss_se, .after = brier)
  bk <- s %>% filter(grepl("^Bookmaker", model)) %>% select(all_of(by), bk_log_loss = log_loss)
  if (nrow(bk) > 0) {
    s <- if (is.null(by)) mutate(s, vs_bookmaker = log_loss - bk$bk_log_loss[1])
         else left_join(s, bk, by = by) %>% mutate(vs_bookmaker = log_loss - bk_log_loss) %>% select(-bk_log_loss)
  }
  s %>% arrange(across(all_of(by)), log_loss)
}

#' Log-loss against xi, one line per (ridge, prior) setting, bookmaker dashed.
plot_backtest <- function(bt) {
  s  <- bt$summary %>% filter(!is.na(xi)) %>%
    mutate(setting = sprintf("ridge=%g prior=%g", ridge, prior))
  bk <- bt$summary %>% filter(grepl("^Bookmaker", model))
  ggplot2::ggplot(s, ggplot2::aes(x = xi, y = log_loss, colour = setting, group = setting)) +
    { if (length(unique(s$xi)) > 1) ggplot2::geom_line() } + ggplot2::geom_point(size = 2) +
    { if (nrow(bk)) ggplot2::geom_hline(yintercept = bk$log_loss[1], linetype = "dashed") } +
    ggplot2::labs(title = "Out-of-sample log-loss by time-decay rate",
                  subtitle = if (nrow(bk)) "Dashed = de-vigged closing odds" else NULL,
                  x = expression(xi ~ "(per day)"), y = "Mean log-loss", colour = NULL) +
    ggplot2::theme_minimal(base_size = 11)
}
