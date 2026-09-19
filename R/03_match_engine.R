# =============================================================================
# 03_match_engine.R  --  From ratings to match probabilities and sampled scores
# -----------------------------------------------------------------------------
# Every fixture is reduced to a (max_goals+1) x (max_goals+1) score-probability
# matrix P, where P[x+1, y+1] = P(home scores x, away scores y). Everything
# else (H/D/A probabilities, expected points, Monte Carlo draws) reads off P,
# so the Dixon-Coles dependence is respected exactly rather than approximated.
# =============================================================================

#' Expected goals for a vector of fixtures.
expected_goals <- function(fit, home, away) {
  hi <- match(home, fit$teams)
  ai <- match(away, fit$teams)
  if (anyNA(hi) || anyNA(ai)) {
    stop("Team(s) not in fitted model: ",
         paste(unique(c(home[is.na(hi)], away[is.na(ai)])), collapse = ", "))
  }
  tibble::tibble(
    home   = home,
    away   = away,
    lambda = exp(fit$attack[hi] + fit$defence[ai] + fit$home),
    mu     = exp(fit$attack[ai] + fit$defence[hi])
  )
}

#' Score-probability matrix for one fixture (rows = home goals, cols = away goals).
score_matrix <- function(lambda, mu, rho = 0, max_goals = 10) {
  g   <- 0:max_goals
  P   <- outer(dpois(g, lambda), dpois(g, mu))        # independent Poisson
  xg  <- matrix(g, nrow = length(g), ncol = length(g)) # home goals by cell
  yg  <- t(xg)                                          # away goals by cell
  tau <- matrix(dc_tau(as.vector(xg), as.vector(yg), lambda, mu, rho), nrow = length(g))
  # tau can dip below 0 for extreme rho (e.g. a wide posterior draw); floor at
  # zero so the matrix remains a valid distribution, then renormalise.
  P   <- pmax(P * tau, 0)
  P / sum(P)                                            # renormalise (truncation + tau)
}

#' Home / draw / away probabilities from a score matrix.
outcome_probs <- function(P) {
  c(p_home = sum(P[lower.tri(P)]),   # row (home goals) > col (away goals)
    p_draw = sum(diag(P)),
    p_away = sum(P[upper.tri(P)]))
}

#' Full pre-match forecast for a fixture list: lambda, mu, P(H/D/A),
#' expected points, most likely score.
match_probs <- function(fit, fixtures, max_goals = 10) {
  xg <- expected_goals(fit, fixtures$home, fixtures$away)
  out <- t(vapply(seq_len(nrow(xg)), function(i) {
    P   <- score_matrix(xg$lambda[i], xg$mu[i], fit$rho, max_goals)
    hda <- outcome_probs(P)
    top <- which(P == max(P), arr.ind = TRUE)[1, ]
    c(hda,
      exp_pts_home = 3 * hda[["p_home"]] + hda[["p_draw"]],
      exp_pts_away = 3 * hda[["p_away"]] + hda[["p_draw"]],
      ml_home_goals = top[["row"]] - 1,
      ml_away_goals = top[["col"]] - 1)
  }, numeric(7)))
  colnames(out) <- c("p_home", "p_draw", "p_away", "exp_pts_home", "exp_pts_away",
                     "ml_home_goals", "ml_away_goals")
  dplyr::bind_cols(fixtures, xg[, c("lambda", "mu")], tibble::as_tibble(out))
}

#' Vectorised Monte Carlo score draws.
#'
#' For each fixture the full score matrix is computed once, then n_sims cells
#' are sampled in a single call (exact Dixon-Coles sampling, no rejection).
#' @return list(hg = matrix[n_fixtures, n_sims], ag = matrix[n_fixtures, n_sims])
sample_scores <- function(fit, fixtures, n_sims = 10000, max_goals = 10) {
  xg <- expected_goals(fit, fixtures$home, fixtures$away)
  nf <- nrow(xg)
  nr <- max_goals + 1
  hg <- matrix(0L, nf, n_sims)
  ag <- matrix(0L, nf, n_sims)
  for (i in seq_len(nf)) {
    P   <- score_matrix(xg$lambda[i], xg$mu[i], fit$rho, max_goals)
    idx <- sample.int(nr * nr, n_sims, replace = TRUE, prob = as.vector(P)) - 1L
    hg[i, ] <- idx %% nr        # column-major: row index = home goals
    ag[i, ] <- idx %/% nr       #               col index = away goals
  }
  list(hg = hg, ag = ag)
}
