# =============================================================================
# 04_monte_carlo.R  --  Simulate N remaining-season schedules
# -----------------------------------------------------------------------------
# Design: no per-simulation loop. All fixtures x all simulations are sampled
# into two integer matrices (home goals, away goals). Points / goals are then
# rolled up per team with a single matrix multiplication against team-by-
# fixture incidence matrices, giving [n_teams x n_sims] matrices of final
# points, GD and GF. 10,000 sims of ~340 fixtures run in a few seconds.
#
# Tie-breaks (Premier League rules): Points -> Goal Difference -> Goals For.
# Anything still tied is broken at random (the PL would use head-to-head /
# a play-off; it is rare enough to be immaterial to the probabilities).
# =============================================================================

#' 0/1 incidence matrix [n_teams x n_items]: M[t, k] = 1 if items[k] == teams[t]
incidence_matrix <- function(items, teams) {
  M <- matrix(0, nrow = length(teams), ncol = length(items))
  M[cbind(match(items, teams), seq_along(items))] <- 1
  M
}

#' Run the Monte Carlo season simulation.
#'
#' @param fit        object from fit_dixon_coles()
#' @param fixtures   remaining fixtures (home, away)
#' @param base_table current standings from current_table(); its team column
#'                   defines the set of teams in the league
#' @param n_sims     number of simulated seasons
#' @param param_draws number of rating vectors drawn from the fit's approximate
#'                   posterior (needs fit$vcov). 1 = use the point estimate for
#'                   every season (original behaviour). K > 1 splits the
#'                   n_sims seasons into K blocks, each simulated under its own
#'                   sampled ratings, so teams with little data (wide posterior)
#'                   get correspondingly wide outcome distributions.
#' @return list of class "season_sim" with:
#'   teams     : team names (row order of every matrix below)
#'   points    : [n_teams x n_sims] final points
#'   gd, gf    : [n_teams x n_sims] final goal difference / goals for
#'   position  : [n_teams x n_sims] final league position (1 = champion)
#'   fixtures  : the fixture list with pre-match H/D/A probabilities attached
run_monte_carlo <- function(fit, fixtures, base_table, n_sims = 10000,
                            seed = 2026, max_goals = 10, param_draws = 1,
                            verbose = TRUE) {
  set.seed(seed)
  teams <- base_table$team
  bad   <- setdiff(unique(c(fixtures$home, fixtures$away)), teams)
  if (length(bad)) stop("Fixture teams not in base table: ", paste(bad, collapse = ", "))
  stopifnot(param_draws >= 1, param_draws <= n_sims)

  t0 <- Sys.time()

  # 1. Sample every remaining score --------------------------------------------
  if (param_draws == 1) {
    s <- sample_scores(fit, fixtures, n_sims, max_goals)
  } else {
    # Block sizes as equal as possible, summing to n_sims
    block <- diff(round(seq(0, n_sims, length.out = param_draws + 1)))
    draws <- sample_ratings(fit, param_draws)
    parts <- Map(function(q, nb) sample_scores(q, fixtures, nb, max_goals), draws, block)
    s <- list(hg = do.call(cbind, lapply(parts, `[[`, "hg")),
              ag = do.call(cbind, lapply(parts, `[[`, "ag")))
  }
  hg <- s$hg
  ag <- s$ag

  # 2. Points per fixture per sim ------------------------------------------------
  home_pts <- 3 * (hg > ag) + (hg == ag)
  away_pts <- 3 * (ag > hg) + (hg == ag)

  # 3. Roll up to team totals via incidence matrices -----------------------------
  H <- incidence_matrix(fixtures$home, teams)    # [n_teams x n_fixtures]
  A <- incidence_matrix(fixtures$away, teams)

  # R recycles a length-n_teams vector down the rows of an [n_teams x n_sims]
  # matrix, so adding base_table columns adds each team's current tally.
  pts <- H %*% home_pts + A %*% away_pts + base_table$pts
  gf  <- H %*% hg       + A %*% ag       + base_table$gf
  ga  <- H %*% ag       + A %*% hg       + base_table$ga
  gd  <- gf - ga

  # 4. Rank with tie-breaks -------------------------------------------------------
  # Encode Pts > GD > GF lexicographically in one numeric key. GD is offset to
  # keep it non-negative; multipliers leave room for the largest realistic values.
  key <- pts * 1e7 + (gd + 500) * 1e4 + gf
  pos <- apply(key, 2, function(k) rank(-k, ties.method = "random"))

  dimnames(pts) <- dimnames(gd) <- dimnames(gf) <- dimnames(pos) <- list(teams, NULL)

  if (verbose) {
    message(sprintf("Simulated %d seasons x %d fixtures (%d rating draw%s) in %.1fs",
                    n_sims, nrow(fixtures), param_draws, if (param_draws > 1) "s" else "",
                    as.numeric(Sys.time() - t0, units = "secs")))
  }

  structure(list(
    teams      = teams,
    points     = pts,
    gd         = gd,
    gf         = gf,
    position   = pos,
    n_sims     = n_sims,
    param_draws = param_draws,
    eff_matches = if (!is.null(fit$eff_matches_team)) unname(fit$eff_matches_team[teams]) else rep(NA_real_, length(teams)),
    base_table = base_table,
    fixtures   = match_probs(fit, fixtures, max_goals)
  ), class = "season_sim")
}

#' Optional parallel variant: splits n_sims across workers with future.apply.
#' Only worthwhile for very large N (>100k) since the base version is already
#' vectorised. Requires install.packages("future.apply").
run_monte_carlo_parallel <- function(fit, fixtures, base_table, n_sims = 100000,
                                     workers = parallel::detectCores() - 1, seed = 2026, ...) {
  if (!requireNamespace("future.apply", quietly = TRUE)) stop("Install future.apply")
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  chunk <- ceiling(n_sims / workers)
  parts <- future.apply::future_lapply(seq_len(workers), function(k) {
    run_monte_carlo(fit, fixtures, base_table, n_sims = chunk, seed = seed + k,
                    verbose = FALSE, ...)
  }, future.seed = TRUE)
  out <- parts[[1]]
  for (m in c("points", "gd", "gf", "position")) {
    out[[m]] <- do.call(cbind, lapply(parts, `[[`, m))
  }
  out$n_sims <- ncol(out$points)
  out
}

print.season_sim <- function(x, ...) {
  cat(sprintf("Season simulation: %d teams, %d sims, %d remaining fixtures\n",
              length(x$teams), x$n_sims, nrow(x$fixtures)))
  invisible(x)
}
