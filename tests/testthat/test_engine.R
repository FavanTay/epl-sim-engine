# Run from project root:  testthat::test_dir("tests/testthat")
library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) source(f)

test_that("score matrix is a valid distribution and reduces to Poisson when rho = 0", {
  P <- score_matrix(1.6, 1.1, rho = -0.1)
  expect_equal(sum(P), 1)
  expect_true(all(P >= 0))
  expect_equal(sum(outcome_probs(P)), 1)
  P0 <- score_matrix(1.6, 1.1, rho = 0, max_goals = 30)
  expect_equal(P0[1, 1], dpois(0, 1.6) * dpois(0, 1.1), tolerance = 1e-8)
})

test_that("rho shifts probability toward 0-0 / 1-1 and away from 1-0 / 0-1 when negative", {
  P0 <- score_matrix(1.5, 1.2, rho = 0)
  Pn <- score_matrix(1.5, 1.2, rho = -0.1)
  expect_gt(Pn[1, 1], P0[1, 1])   # 0-0 up
  expect_gt(Pn[2, 2], P0[2, 2])   # 1-1 up
  expect_lt(Pn[2, 1], P0[2, 1])   # 1-0 down
})

test_that("Dixon-Coles fit recovers synthetic strengths in the right order", {
  syn <- make_synthetic_data(n_past_seasons = 3, rounds_played = 10, seed = 7)
  fit <- fit_dixon_coles(syn$results, xi = 0, verbose = FALSE)
  expect_equal(fit$convergence, 0)
  expect_equal(unname(sum(fit$attack)), 0, tolerance = 1e-8)
  expect_gt(cor(fit$attack[syn$truth$team], syn$truth$attack), 0.8)
  expect_gt(cor(fit$defence[syn$truth$team], syn$truth$defence), 0.8)
  expect_equal(fit$home, syn$home_adv, tolerance = 0.1)
})

test_that("sampled H/D/A frequencies match analytic probabilities", {
  syn <- make_synthetic_data(seed = 3)
  fit <- fit_dixon_coles(syn$results, verbose = FALSE)
  fx  <- tibble(home = "Team 01", away = "Team 02")
  s   <- sample_scores(fit, fx, n_sims = 200000)
  emp <- c(mean(s$hg > s$ag), mean(s$hg == s$ag), mean(s$hg < s$ag))
  ana <- unname(outcome_probs(score_matrix(expected_goals(fit, "Team 01", "Team 02")$lambda,
                                           expected_goals(fit, "Team 01", "Team 02")$mu, fit$rho)))
  expect_equal(emp, ana, tolerance = 0.01)
})

test_that("tie-breaks follow Points > GD > GF", {
  base <- tibble(team = c("A", "B", "C"), played = 0L, won = 0L, drawn = 0L, lost = 0L,
                 gf = c(10L, 12L, 12L), ga = c(0L, 5L, 2L), gd = c(10L, 7L, 10L), pts = c(9L, 9L, 9L))
  # A and C tied on pts and GD; C has more GF -> C first. B lowest GD -> last.
  fit <- list(attack = c(A = 0, B = 0, C = 0), defence = c(A = 0, B = 0, C = 0),
              home = 0, rho = 0, teams = c("A", "B", "C"))
  fx  <- tibble(home = character(0), away = character(0))
  sim <- run_monte_carlo(fit, fx, base, n_sims = 5, verbose = FALSE)
  expect_equal(unname(sim$position[, 1]), c(2, 3, 1))
})

test_that("Monte Carlo output is internally consistent", {
  syn <- make_synthetic_data(seed = 11)
  cur <- filter(syn$results, season == "current")
  bt  <- current_table(cur)
  fx  <- derive_remaining_fixtures(cur)
  expect_equal(nrow(cur) + nrow(fx), 380)
  fit <- fit_dixon_coles(syn$results, verbose = FALSE)
  sim <- run_monte_carlo(fit, fx, bt, n_sims = 300, verbose = FALSE)
  # every simulated season is a permutation of 1..20
  expect_true(all(apply(sim$position, 2, function(p) isTRUE(all.equal(unname(sort(p)), 1:20)))))
  # total points per sim = current total + 3*wins + 2*draws in remaining fixtures (2..3 per match)
  tot <- colSums(sim$points) - sum(bt$pts)
  expect_true(all(tot >= 2 * nrow(fx) & tot <= 3 * nrow(fx)))
  # goal totals balance
  expect_equal(colSums(sim$gd), rep(0, 300))
  pp <- position_probabilities(sim)
  expect_equal(pp %>% group_by(team) %>% summarise(s = sum(prob)) %>% pull(s), rep(1, 20))
})

test_that("de-vig and scoring behave", {
  p <- devig(c(2.0, 1.5), c(3.5, 4.0), c(4.0, 7.0))
  expect_equal(unname(rowSums(p)), c(1, 1))
  expect_true(all(p[1, ] < c(0.5, 1/3.5, 0.25)))  # margin removed
  s <- score_forecasts(c(0.5, 0.2), c(0.3, 0.3), c(0.2, 0.5), hg = c(2, 0), ag = c(1, 1))
  expect_equal(s$log_loss, -log(c(0.5, 0.5)))
  expect_equal(s$brier[1], (0.5 - 1)^2 + 0.3^2 + 0.2^2)
})

test_that("rolling backtest runs end-to-end on synthetic data with fake odds", {
  syn <- make_synthetic_data(n_past_seasons = 2, rounds_played = 38, seed = 5)
  res <- syn$results %>%
    mutate(season = ifelse(season == "current", "test", season),
           # fake odds: noisy version of the truth, with a 5% margin
           odds_h = 1 / (pmin(pmax(0.45 + rnorm(n(), 0, 0.05), 0.05), 0.9) * 1.05),
           odds_d = 1 / (0.26 * 1.05),
           odds_a = 1 / (pmin(pmax(0.29 + rnorm(n(), 0, 0.05), 0.05), 0.9) * 1.05))
  bt <- run_backtest(res, test_seasons = "test", xi_grid = c(0, 0.003), ridge_grid = 1,
                     step_days = 60, verbose = FALSE)
  expect_setequal(unique(bt$matches$model),
                  c("DC xi=0.0000 ridge=1 prior=0", "DC xi=0.0030 ridge=1 prior=0", "Naive base rates",
                    "Bookmaker (de-vigged closing)"))
  # every model scored on the same matches
  expect_equal(length(unique(table(bt$matches$model))), 1)
  expect_true(all(is.finite(bt$matches$log_loss)))
  expect_true("vs_bookmaker" %in% names(bt$summary))
  # the model has real signal: beats naive base rates
  s <- bt$summary
  expect_lt(s$log_loss[s$model == "DC xi=0.0000 ridge=1 prior=0"], s$log_loss[s$model == "Naive base rates"])
})

test_that("pseudo-match prior only moves low-data teams and is off by default", {
  syn <- make_synthetic_data(n_past_seasons = 2, rounds_played = 4, seed = 9)
  new <- c("Team 18", "Team 19", "Team 20")
  res <- syn$results %>%
    filter(!(season != "current" & (home %in% new | away %in% new))) %>%
    mutate(hg = ifelse(season == "current" & home %in% new, 3L, hg),
           ag = ifelse(season == "current" & away %in% new, 3L, ag))
  f0 <- fit_dixon_coles(res, prior_matches = 0,  verbose = FALSE)
  f1 <- fit_dixon_coles(res, prior_matches = 20, verbose = FALSE)
  expect_true(all(f1$eff_matches_team[new] < 10))
  expect_true(all(f1$eff_matches_team[setdiff(f1$teams, new)] > 20))
  # inflated new-team attack is pulled toward the prior mean
  expect_true(all(f1$attack[new] < f0$attack[new] - 0.2))
  # default (prior_matches = 0) reproduces the unpenalised fit
  f_default <- fit_dixon_coles(res, verbose = FALSE)
  expect_equal(f_default$attack, f0$attack)
})

test_that("Stage 6a: vcov is returned and param_draws = 1 reproduces the original output", {
  syn <- make_synthetic_data(seed = 11)
  cur <- filter(syn$results, season == "current")
  bt  <- current_table(cur)
  fx  <- derive_remaining_fixtures(cur)
  fit <- fit_dixon_coles(syn$results, verbose = FALSE)
  expect_equal(dim(fit$vcov), c(fit$n_free, fit$n_free))
  expect_true(isSymmetric(fit$vcov))
  expect_true(all(diag(fit$vcov) > 0))
  fit_nohess <- fit_dixon_coles(syn$results, hessian = FALSE, verbose = FALSE)
  expect_null(fit_nohess$vcov)
  expect_equal(fit_nohess$attack, fit$attack)
  s1 <- run_monte_carlo(fit, fx, bt, n_sims = 200, seed = 1, verbose = FALSE)
  s2 <- run_monte_carlo(fit, fx, bt, n_sims = 200, seed = 1, param_draws = 1, verbose = FALSE)
  expect_identical(s1$points, s2$points)
  expect_error(run_monte_carlo(fit_nohess, fx, bt, n_sims = 50, param_draws = 5, verbose = FALSE), "vcov")
})

test_that("Stage 6a: posterior draws widen outcomes most for low-data teams", {
  syn <- make_synthetic_data(n_past_seasons = 2, rounds_played = 4, seed = 9)
  new <- c("Team 18", "Team 19", "Team 20")
  res <- syn$results %>% filter(!(season != "current" & (home %in% new | away %in% new)))
  cur <- filter(res, season == "current")
  bt  <- current_table(cur, teams = syn$truth$team)
  fx  <- derive_remaining_fixtures(cur, teams = syn$truth$team)
  fit <- fit_dixon_coles(res, verbose = FALSE)
  unc <- rating_uncertainty(fit)
  expect_true(all(unc$attack_sd[unc$team %in% new] > 2 * median(unc$attack_sd[!unc$team %in% new])))
  draws <- sample_ratings(fit, 50)
  expect_true(all(sapply(draws, function(q) abs(sum(q$attack)) < 1e-10)))
  s1 <- run_monte_carlo(fit, fx, bt, n_sims = 4000, seed = 3, param_draws = 1,   verbose = FALSE)
  sk <- run_monte_carlo(fit, fx, bt, n_sims = 4000, seed = 3, param_draws = 100, verbose = FALSE)
  expect_equal(ncol(sk$points), 4000)
  sd1 <- apply(s1$points, 1, sd); sdk <- apply(sk$points, 1, sd)
  widen <- (sdk - sd1)[bt$team]
  # spread grows for every team, and grows more for the 3 low-data teams than the typical team
  expect_true(all(widen > 0))
  expect_gt(min(widen[new]), median(widen[!names(widen) %in% new]))
  # expected points barely move for well-measured teams (low-data teams carry
  # MC noise from the 100 rating draws, so they get a looser bound)
  shift <- abs(rowMeans(sk$points) - rowMeans(s1$points))[bt$team]
  expect_lt(max(shift[!names(shift) %in% new]), 2.5)
  expect_lt(max(shift[new]), 6)
})

test_that("Stage 7: upcoming-fixture loader parses football-data fixtures.csv and attaches probabilities", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "Div,Date,Time,HomeTeam,AwayTeam,B365H,B365D,B365A",
    "E0,26/09/2026,15:00,Team 01,Team 02,2.10,3.40,3.60",
    "E0,27/09/2026,16:30,Team 03,Team 04,,,",
    "E1,26/09/2026,15:00,Somewhere,Elsewhere,1.5,4,6",
    "E0,26/09/2026,12:30,Unknown FC,Team 05,2,3,4"), tmp)
  syn <- make_synthetic_data(seed = 2)
  fit <- fit_dixon_coles(syn$results, hessian = FALSE, verbose = FALSE)
  expect_warning(fx <- load_upcoming_fixtures(tmp, teams = fit$teams), "Unknown FC")
  expect_equal(nrow(fx), 2)                          # E1 row and unknown team dropped
  expect_equal(fx$date[1], as.Date("2026-09-26"))
  up <- upcoming_fixture_probs(fit, fx)
  expect_true(all(c("p_home", "p_draw", "p_away", "mkt_home", "ml_home_goals") %in% names(up)))
  expect_equal(up$mkt_home[1] + up$mkt_draw[1] + up$mkt_away[1], 1)
  expect_true(is.na(up$mkt_home[2]))                 # no odds on row 2
  expect_null(upcoming_fixture_probs(fit, NULL))
  expect_equal(matchweek_number(tibble(team = "x", played = c(4L, 5L, 4L))), 5L)
})

test_that("Stage 7: render_report degrades gracefully and renders when Quarto is present", {
  # Missing qmd -> NULL, no error
  expect_message(r <- render_report(1, qmd = "does_not_exist.qmd"), "skipping")
  expect_null(r)
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  # Build a minimal real output set on synthetic data, then render into a temp dir
  syn <- make_synthetic_data(seed = 11)
  cur <- filter(syn$results, season == "current"); bt <- current_table(cur)
  fx  <- derive_remaining_fixtures(cur)
  fit <- fit_dixon_coles(syn$results, verbose = FALSE)
  sim <- run_monte_carlo(fit, fx, bt, n_sims = 500, seed = 1, param_draws = 10, verbose = FALSE)
  out_dir <- file.path(tempdir(), "rep_out"); dir.create(out_dir, showWarnings = FALSE)
  saveRDS(list(fit = fit, sim = sim, config = list(xi = 0.0018, seasons = "synthetic")),
          file.path(out_dir, "simulation.rds"))
  save_all_plots(sim, summarise_simulation(sim), dir = out_dir)
  html <- render_report(matchweek_number(bt), run_date = as.Date("2026-09-19"),
                        qmd = file.path(root, "report.qmd"), reports_dir = file.path(out_dir, "reports"),
                        rds = file.path(out_dir, "simulation.rds"),
                        upcoming_csv = file.path(out_dir, "none.csv"), output_dir = out_dir)
  expect_true(!is.null(html) && file.exists(html))
  expect_match(basename(html), "^MW\\d{2}_2026-09-19\\.html$")
  txt <- readChar(html, file.info(html)$size)
  expect_true(all(sapply(c("Executive summary", "Season forecast", "Upcoming fixtures",
                           "Final-position probabilities", "Methodology"), grepl, txt, fixed = TRUE)))
  expect_gte(lengths(regmatches(txt, gregexpr("data:image/png", txt))), 4)   # plots embedded
})
