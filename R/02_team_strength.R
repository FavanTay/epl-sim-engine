# =============================================================================
# 02_team_strength.R  --  Dixon-Coles (1997) team-strength model
# -----------------------------------------------------------------------------
# Model:
#   X_ij ~ Poisson(lambda),  lambda = exp(attack_i + defence_j + home)
#   Y_ij ~ Poisson(mu),      mu     = exp(attack_j + defence_i)
#   P(x,y) = tau(x,y) * Pois(x|lambda) * Pois(y|mu)
# where tau adjusts the four low-scoring cells (0-0, 1-0, 0-1, 1-1) by rho,
# capturing the empirical dependence between home and away goals.
#
# Identifiability: sum(attack) = 0 (the last attack parameter is minus the sum
# of the others). "defence" is a log-multiplier on goals CONCEDED, so a LOWER
# value = a BETTER defence.
#
# Recent-form weighting: each match gets weight exp(-xi * days_ago). With
# xi = 0.0018 / day a match is worth ~50% after ~13 months. This is the
# Dixon-Coles "half-week" xi = 0.0065 converted to days.
# =============================================================================

#' Dixon-Coles low-score correction factor. Vectorised over x, y.
#' lambda, mu may be scalars (recycled) or vectors the same length as x.
dc_tau <- function(x, y, lambda, mu, rho) {
  n <- length(x)
  lambda <- rep_len(lambda, n)
  mu     <- rep_len(mu, n)
  out <- rep(1, n)
  i <- x == 0 & y == 0; out[i] <- 1 - lambda[i] * mu[i] * rho
  i <- x == 0 & y == 1; out[i] <- 1 + lambda[i] * rho
  i <- x == 1 & y == 0; out[i] <- 1 + mu[i] * rho
  i <- x == 1 & y == 1; out[i] <- 1 - rho
  out
}

#' Fit the Dixon-Coles model by (time-weighted) maximum likelihood.
#'
#' @param results   tibble in canonical schema (date, home, away, hg, ag)
#' @param xi        time-decay rate per day (0 = no decay)
#' @param ref_date  date from which "days ago" is measured (default: latest match)
#' @param ridge     global L2 penalty on attack/defence parameters (applies to
#'                  every team equally; backtests show values >1 hurt).
#' @param prior_matches  pseudo-match prior for low-data teams. Every team is
#'                  treated as if it had at least this many (time-weighted)
#'                  matches of evidence; the shortfall is filled with
#'                  pseudo-matches at `prior_mean`. Teams with more real
#'                  evidence than this are untouched. 0 = off (default).
#' @param prior_mean  named vector c(attack=, defence=) on the log scale: the
#'                  profile a low-data team is pulled toward. Default is a
#'                  typical newly promoted side (attack x0.85, defence x1.2).
#' @param hessian   if TRUE, compute the observed information at the optimum
#'                  and return its inverse as `vcov` (approximate posterior
#'                  covariance of the free parameters). Costs a few seconds;
#'                  the backtest turns it off.
#' @return list of class "dc_fit": attack, defence (named vectors), home, rho,
#'         teams, xi, ref_date, loglik, convergence, eff_matches_team, and
#'         (if hessian) par (free-parameter vector) and vcov.
fit_dixon_coles <- function(results, xi = 0.0018, ref_date = max(results$date),
                            ridge = 0, prior_matches = 0,
                            prior_mean = c(attack = log(0.85), defence = log(1.2)),
                            hessian = TRUE, verbose = TRUE) {
  stopifnot(all(c("date", "home", "away", "hg", "ag") %in% names(results)))

  teams <- sort(unique(c(results$home, results$away)))
  n  <- length(teams)
  hi <- match(results$home, teams)
  ai <- match(results$away, teams)
  hg <- results$hg
  ag <- results$ag
  w  <- exp(-xi * as.numeric(ref_date - results$date))

  # Time-weighted matches per team, and the pseudo-match shortfall that the
  # prior has to fill. Fisher information for a Poisson log-rate is ~lambda
  # per observation, so one pseudo-match contributes ~lambda_bar to the
  # curvature of the penalty: penalty_i = 0.5 * shortfall_i * lambda_bar * dev_i^2.
  eff_team   <- as.vector(tapply(c(w, w), c(hi, ai), sum))
  eff_team[is.na(eff_team)] <- 0
  shortfall  <- pmax(prior_matches - eff_team, 0)
  lambda_bar <- mean(c(hg, ag))
  prior_w    <- 0.5 * shortfall * lambda_bar

  # Parameter vector layout:
  #   [1 : n-1]      attack (team n is implied by the sum-to-zero constraint)
  #   [n : 2n-1]     defence
  #   [2n]           home advantage (log scale)
  #   [2n+1]         rho
  unpack <- function(p) {
    list(
      attack  = c(p[1:(n - 1)], -sum(p[1:(n - 1)])),
      defence = p[n:(2 * n - 1)],
      home    = p[2 * n],
      rho     = p[2 * n + 1]
    )
  }

  neg_loglik <- function(p) {
    q   <- unpack(p)
    lam <- exp(q$attack[hi] + q$defence[ai] + q$home)
    mu  <- exp(q$attack[ai] + q$defence[hi])
    tau <- dc_tau(hg, ag, lam, mu, q$rho)
    if (any(tau <= 0)) return(1e12)             # rho outside the valid region
    ll <- sum(w * (log(tau) + dpois(hg, lam, log = TRUE) + dpois(ag, mu, log = TRUE)))
    penalty <- ridge * (sum(q$attack^2) + sum(q$defence^2)) +
      sum(prior_w * ((q$attack - prior_mean[["attack"]])^2 +
                     (q$defence - prior_mean[["defence"]])^2))
    -ll + penalty
  }

  p0    <- c(rep(0, n - 1), rep(0, n), 0.25, -0.05)
  lower <- c(rep(-Inf, 2 * n), -0.5)
  upper <- c(rep( Inf, 2 * n),  0.5)

  opt <- optim(p0, neg_loglik, method = "L-BFGS-B", lower = lower, upper = upper,
               control = list(maxit = 1000))
  if (opt$convergence != 0 && verbose) {
    warning("optim did not converge cleanly (code ", opt$convergence, "): ", opt$message)
  }

  # Approximate posterior covariance: inverse of the observed information
  # (Hessian of the negative penalised log-likelihood) at the optimum.
  vcov <- NULL
  if (hessian) {
    H <- optimHess(opt$par, neg_loglik)
    vcov <- tryCatch(solve(H), error = function(e) NULL)
    if (is.null(vcov) || any(diag(vcov) <= 0)) {
      if (verbose) warning("Hessian not positive definite; using generalised inverse")
      vcov <- MASS::ginv(H)
      diag(vcov) <- pmax(diag(vcov), 1e-8)
    }
    vcov <- (vcov + t(vcov)) / 2                # enforce symmetry
  }

  q <- unpack(opt$par)
  fit <- list(
    attack      = setNames(q$attack, teams),
    defence     = setNames(q$defence, teams),
    home        = q$home,
    rho         = q$rho,
    teams       = teams,
    xi          = xi,
    ref_date    = ref_date,
    n_matches   = nrow(results),
    eff_matches = sum(w),                       # effective sample size after decay
    eff_matches_team = setNames(eff_team, teams),
    prior_matches = prior_matches,
    loglik      = -opt$value,
    convergence = opt$convergence,
    par         = opt$par,                      # free parameters (see layout above)
    vcov        = vcov,
    n_free      = 2 * n + 1
  )
  class(fit) <- "dc_fit"
  if (verbose) {
    message(sprintf("Dixon-Coles fit: %d teams, %d matches (%.0f effective), home adv x%.3f, rho = %.3f",
                    n, nrow(results), sum(w), exp(q$home), q$rho))
  }
  fit
}

#' Unpack a free-parameter vector into named attack / defence / home / rho.
unpack_dc_params <- function(p, teams) {
  n <- length(teams)
  list(
    attack  = setNames(c(p[1:(n - 1)], -sum(p[1:(n - 1)])), teams),
    defence = setNames(p[n:(2 * n - 1)], teams),
    home    = p[2 * n],
    rho     = min(max(p[2 * n + 1], -0.5), 0.5)   # keep rho inside the fitted bounds
  )
}

#' Draw k rating vectors from the approximate posterior N(par, vcov).
#' Each element is a list usable wherever a dc_fit is (attack, defence, home,
#' rho, teams), so it can be passed straight to sample_scores().
sample_ratings <- function(fit, k) {
  if (is.null(fit$vcov)) stop("fit has no vcov; refit with hessian = TRUE")
  draws <- MASS::mvrnorm(k, mu = fit$par, Sigma = fit$vcov)
  if (k == 1) draws <- matrix(draws, nrow = 1)
  lapply(seq_len(k), function(i) {
    q <- unpack_dc_params(draws[i, ], fit$teams)
    q$teams <- fit$teams
    q
  })
}

#' Posterior standard deviation of each team's attack and defence rating
#' (attack SD for the last team is derived from the sum-to-zero constraint).
rating_uncertainty <- function(fit) {
  if (is.null(fit$vcov)) stop("fit has no vcov; refit with hessian = TRUE")
  n <- length(fit$teams)
  J <- rbind(diag(n - 1), rep(-1, n - 1))       # attack = J %*% free attack params
  V_att <- J %*% fit$vcov[1:(n - 1), 1:(n - 1)] %*% t(J)
  tibble::tibble(
    team        = fit$teams,
    eff_matches = unname(fit$eff_matches_team),
    attack_sd   = sqrt(pmax(diag(V_att), 0)),
    defence_sd  = sqrt(pmax(diag(fit$vcov)[n:(2 * n - 1)], 0))
  ) %>% dplyr::arrange(eff_matches)
}

#' Human-readable strength table. attack/defence are shown as multipliers on
#' the league-average goal rate (defence < 1 = concedes fewer than average).
strength_table <- function(fit, teams = fit$teams) {
  tibble::tibble(
    team            = teams,
    attack_mult     = exp(fit$attack[teams]),
    defence_mult    = exp(fit$defence[teams]),
    net_rating      = fit$attack[teams] - fit$defence[teams]   # log-scale overall strength
  ) %>% dplyr::arrange(dplyr::desc(net_rating))
}

print.dc_fit <- function(x, ...) {
  cat("Dixon-Coles fit\n")
  cat(sprintf("  teams: %d   matches: %d (effective %.0f)   xi: %.4f/day\n",
              length(x$teams), x$n_matches, x$eff_matches, x$xi))
  cat(sprintf("  home advantage: x%.3f   rho: %.3f   loglik: %.1f\n",
              exp(x$home), x$rho, x$loglik))
  invisible(x)
}
