# =============================================================================
# 05_summarise.R  --  Aggregate simulation output into forecast tables
# =============================================================================

#' One row per team: current points, expected finish, and outcome probabilities (%).
#' `low_data` flags teams whose rating rests on fewer than `low_data_matches`
#' effective matches (e.g. newly promoted sides early in the season): their
#' tail probabilities are wide and should be read with that in mind.
summarise_simulation <- function(sim, top_n = c(4, 6), relegation_n = 3, low_data_matches = 10) {
  n_teams <- length(sim$teams)
  pos <- sim$position
  pts <- sim$points

  out <- tibble::tibble(
    team         = sim$teams,
    played       = sim$base_table$played,
    eff_matches  = round(sim$eff_matches, 1),
    low_data     = !is.na(sim$eff_matches) & sim$eff_matches < low_data_matches,
    current_pts  = sim$base_table$pts,
    exp_pts      = rowMeans(pts),
    pts_p05      = apply(pts, 1, quantile, probs = 0.05),
    pts_p95      = apply(pts, 1, quantile, probs = 0.95),
    exp_pos      = rowMeans(pos),
    p_title      = 100 * rowMeans(pos == 1),
    p_relegation = 100 * rowMeans(pos > n_teams - relegation_n)
  )
  for (k in top_n) out[[paste0("p_top", k)]] <- 100 * rowMeans(pos <= k)

  out %>%
    dplyr::relocate(p_relegation, .after = dplyr::last_col()) %>%
    dplyr::arrange(exp_pos)
}

#' Long table of P(team finishes in position k), for heatmaps.
position_probabilities <- function(sim) {
  n_teams <- length(sim$teams)
  counts  <- t(apply(sim$position, 1, tabulate, nbins = n_teams))   # [n_teams x n_teams]
  tibble::as_tibble(counts / sim$n_sims, .name_repair = ~ as.character(seq_len(n_teams))) %>%
    dplyr::mutate(team = sim$teams, .before = 1) %>%
    tidyr::pivot_longer(-team, names_to = "position", values_to = "prob") %>%
    dplyr::mutate(position = as.integer(position))
}

#' Long table of simulated final points, for distribution plots.
points_long <- function(sim) {
  tibble::tibble(
    team = rep(sim$teams, times = sim$n_sims),
    sim_id = rep(seq_len(sim$n_sims), each = length(sim$teams)),
    pts  = as.vector(sim$points)
  )
}

#' Pretty-print the headline table with rounded percentages.
format_summary <- function(summary_tbl, digits = 1) {
  summary_tbl %>%
    dplyr::mutate(dplyr::across(c(exp_pts, exp_pos), ~ round(.x, 1)),
                  dplyr::across(dplyr::starts_with("p_"), ~ round(.x, digits)))
}
