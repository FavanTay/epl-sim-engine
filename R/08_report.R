# =============================================================================
# 08_report.R  --  Stage 7: render the weekly Quarto report
# -----------------------------------------------------------------------------
# report.qmd (project root) reads output/simulation.rds and the CSV/PNG files
# the pipeline wrote, and renders to output/reports/MW{X}_{date}.html as a
# single self-contained file. Requires the Quarto CLI (https://quarto.org);
# if it is not installed the pipeline says so and continues.
# =============================================================================

#' Matchweek number = most matches any team has played.
matchweek_number <- function(base_table) as.integer(max(base_table$played))

#' Attach model probabilities (and de-vigged market probabilities when odds
#' are present) to a fixture list.
upcoming_fixture_probs <- function(fit, fixtures, max_goals = 10) {
  if (is.null(fixtures) || nrow(fixtures) == 0) return(NULL)
  mp <- match_probs(fit, fixtures %>% dplyr::select(home, away), max_goals) %>%
    dplyr::select(-home, -away)
  out <- dplyr::bind_cols(fixtures, mp)
  if (all(c("odds_h", "odds_d", "odds_a") %in% names(out)) && any(!is.na(out$odds_h))) {
    ok <- !is.na(out$odds_h) & !is.na(out$odds_d) & !is.na(out$odds_a)
    inv <- cbind(1 / out$odds_h, 1 / out$odds_d, 1 / out$odds_a)
    mk  <- inv / rowSums(inv)
    out$mkt_home <- ifelse(ok, mk[, 1], NA_real_)
    out$mkt_draw <- ifelse(ok, mk[, 2], NA_real_)
    out$mkt_away <- ifelse(ok, mk[, 3], NA_real_)
  }
  out
}

#' Render report.qmd. Returns the output path, or NULL if Quarto is missing
#' or rendering failed (never stops the pipeline).
render_report <- function(matchweek, run_date = Sys.Date(), qmd = "report.qmd",
                          reports_dir = "output/reports", rds = "output/simulation.rds",
                          upcoming_csv = "output/upcoming_fixtures.csv", output_dir = "output",
                          quiet = TRUE) {
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) {
    message("Quarto CLI not found; skipping report. Install from https://quarto.org/docs/get-started/")
    return(invisible(NULL))
  }
  if (!file.exists(qmd)) { message("No ", qmd, " found; skipping report."); return(invisible(NULL)) }
  dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
  fname <- sprintf("MW%02d_%s.html", matchweek, format(as.Date(run_date), "%Y-%m-%d"))

  # Quarto resolves its support files relative to the working directory, so
  # render from the qmd's own directory and pass every other path as absolute.
  qmd          <- normalizePath(qmd)
  reports_dir  <- normalizePath(reports_dir)
  output_dir   <- normalizePath(output_dir, mustWork = FALSE)
  rds          <- normalizePath(rds, mustWork = FALSE)
  upcoming_csv <- normalizePath(upcoming_csv, mustWork = FALSE)
  old_wd <- setwd(dirname(qmd)); on.exit(setwd(old_wd), add = TRUE)

  args <- c("render", shQuote(basename(qmd)),
            "--output-dir", shQuote(reports_dir), "--output", shQuote(fname),
            "-P", shQuote(paste0("rds:", rds)),
            "-P", shQuote(paste0("upcoming_csv:", upcoming_csv)),
            "-P", shQuote(paste0("output_dir:", output_dir)),
            "-P", shQuote(paste0("matchweek:", matchweek)),
            "-P", shQuote(paste0("run_date:", format(as.Date(run_date), "%Y-%m-%d"))),
            if (quiet) "--quiet")
  status <- system2(quarto, args, stdout = if (quiet) FALSE else "", stderr = "")
  out <- file.path(reports_dir, fname)
  # Quarto copies referenced images beside the output even though they are
  # embedded; remove that duplicate folder so reports/ holds only HTML files.
  stray <- file.path(reports_dir, basename(output_dir))
  if (dir.exists(stray) && stray != output_dir) unlink(stray, recursive = TRUE)
  if (status != 0 || !file.exists(out)) {
    message("Quarto render failed (exit ", status, "); see messages above.")
    return(invisible(NULL))
  }
  message("Report written: ", out)
  invisible(out)
}
