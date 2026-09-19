# =============================================================================
# 06_plots.R  --  ggplot2 visualisations
# =============================================================================
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

#' Heatmap: P(team finishes in position k). Teams ordered by expected position.
plot_position_heatmap <- function(sim, summary_tbl = summarise_simulation(sim)) {
  pp <- position_probabilities(sim) %>%
    mutate(team = factor(team, levels = rev(summary_tbl$team)),
           label = ifelse(prob >= 0.01, sprintf("%.0f", 100 * prob), ""))
  n_teams <- length(sim$teams)

  ggplot(pp, aes(x = position, y = team, fill = prob)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = label), size = 2.8,
              colour = ifelse(pp$prob > 0.35, "white", "grey15")) +
    # Zone dividers: after 4th (UCL), 6th (Europe), before 18th (relegation)
    geom_vline(xintercept = c(4.5, 6.5, n_teams - 2.5), linetype = "dashed", colour = "grey30") +
    scale_x_continuous(breaks = seq_len(n_teams), expand = c(0, 0)) +
    scale_fill_gradient(low = "#f7fbff", high = "#08306b", labels = scales::percent,
                        name = "Probability") +
    labs(title = "Final league position probabilities",
         subtitle = sprintf("%s simulated seasons | Dixon-Coles model", format(sim$n_sims, big.mark = ",")),
         x = "Final position", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "right")
}

#' Ridge plot of final-points distributions (falls back to violins if ggridges
#' is not installed).
plot_points_distribution <- function(sim, summary_tbl = summarise_simulation(sim)) {
  pl <- points_long(sim) %>%
    mutate(team = factor(team, levels = rev(summary_tbl$team)))

  base <- ggplot(pl, aes(x = pts, y = team)) +
    labs(title = "Distribution of final points",
         subtitle = sprintf("%s simulated seasons", format(sim$n_sims, big.mark = ",")),
         x = "Final points", y = NULL) +
    theme_minimal(base_size = 11)

  if (requireNamespace("ggridges", quietly = TRUE)) {
    base +
      ggridges::geom_density_ridges(aes(fill = team), stat = "binline", binwidth = 1,
                                    scale = 1.6, alpha = 0.8, colour = "grey40", show.legend = FALSE) +
      ggridges::theme_ridges(grid = TRUE) +
      theme(axis.title.x = element_text(hjust = 0.5))
  } else {
    base + geom_violin(aes(fill = team), alpha = 0.7, show.legend = FALSE)
  }
}

#' Bar chart of title / top-4 / relegation probabilities for teams with a
#' non-trivial chance.
plot_outcome_bars <- function(summary_tbl, outcome = c("p_title", "p_top4", "p_relegation"),
                              min_prob = 0.5) {
  outcome <- match.arg(outcome)
  ttl <- c(p_title = "Title", p_top4 = "Top 4 (Champions League)", p_relegation = "Relegation")[outcome]
  d <- summary_tbl %>% filter(.data[[outcome]] >= min_prob) %>%
    arrange(.data[[outcome]]) %>%
    mutate(team = factor(team, levels = team))

  ggplot(d, aes(x = .data[[outcome]], y = team)) +
    geom_col(fill = if (outcome == "p_relegation") "#b2182b" else "#2166ac") +
    geom_text(aes(label = sprintf("%.1f%%", .data[[outcome]])), hjust = -0.1, size = 3.2) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = paste(ttl, "probability"), x = "%", y = NULL) +
    theme_minimal(base_size = 11)
}

#' Save all standard figures to a directory.
save_all_plots <- function(sim, summary_tbl, dir = "output", width = 9, height = 7) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(dir, "position_heatmap.png"), plot_position_heatmap(sim, summary_tbl),
         width = width, height = height, dpi = 150)
  ggsave(file.path(dir, "points_distribution.png"), plot_points_distribution(sim, summary_tbl),
         width = width, height = height, dpi = 150)
  ggsave(file.path(dir, "title_race.png"), plot_outcome_bars(summary_tbl, "p_title"),
         width = 7, height = 4, dpi = 150)
  ggsave(file.path(dir, "relegation.png"), plot_outcome_bars(summary_tbl, "p_relegation"),
         width = 7, height = 4, dpi = 150)
  invisible(list.files(dir, pattern = "png$", full.names = TRUE))
}
