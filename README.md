# EPL Match Prediction Engine & Monte Carlo Simulation

Predicts Premier League match outcomes with a time-weighted Dixon-Coles model
and simulates the remainder of the season 10,000 times to produce title,
Top-4 / Top-6 and relegation probabilities.

## Quick start

```r
install.packages(c("dplyr", "tidyr", "readr", "tibble", "lubridate",
                   "ggplot2", "scales", "ggridges", "testthat"))
```

```bash
# Real data (downloads football-data.co.uk CSVs into data/raw/ on first run)
Rscript run_simulation.R

# Synthetic data, no network needed
USE_SYNTHETIC=1 Rscript run_simulation.R

# Tests
Rscript -e 'testthat::test_dir("tests/testthat")'
```

Outputs land in `output/`: `season_forecast.csv`, `position_probabilities.csv`,
`fixture_probabilities.csv`, `upcoming_fixtures.csv`, four PNG figures,
`simulation.rds`, and a self-contained weekly report at
`output/reports/MW{XX}_{date}.html` (needs the [Quarto CLI](https://quarto.org/docs/get-started/)
plus `install.packages(c("knitr", "kableExtra"))`; set `SKIP_REPORT=1` to skip it).

## Data

**Results (required):** [football-data.co.uk](https://www.football-data.co.uk/englandm.php),
file `E0.csv` per season, downloaded automatically by `download_football_data()`.
Season codes are the two 2-digit years: `2627` = 2026/27. Only `Date, HomeTeam,
AwayTeam, FTHG, FTAG` are used; the odds columns are useful later for
benchmarking the model against the market.

**Remaining fixtures:** derived automatically (every home/away pairing not yet
played). If you want real match dates, download the season CSV from
[fixturedownload.com](https://fixturedownload.com/results/epl-2026) and set
`config$fixtures_file`. Team names differ between the two sites; extend
`fixturedownload_name_map()` in `R/01_load_data.R` if the loader reports an
unmapped name.

**xG (later):** the `worldfootballR` package pulls Understat / FBref xG into R.

## Modules

| File | Responsibility |
|---|---|
| `R/01_load_data.R` | Download & parse results, derive remaining fixtures, current table, synthetic data |
| `R/02_team_strength.R` | Dixon-Coles MLE fit with exponential time decay, ridge shrinkage |
| `R/03_match_engine.R` | Expected goals, score matrix, H/D/A probabilities, vectorised score sampling |
| `R/04_monte_carlo.R` | N-season simulation via incidence-matrix algebra; Pts > GD > GF ranking |
| `R/05_summarise.R` | Forecast table, position-probability matrix, long points data |
| `R/06_plots.R` | ggplot2 heatmap, ridge plot, outcome bar charts |
| `R/07_backtest.R` | Rolling-origin backtest: xi/ridge grid vs de-vigged closing odds |
| `R/08_report.R` | Upcoming-fixture probabilities and Quarto report rendering |
| `report.qmd` | Weekly report template (executive summary, forecast table, fixtures, plots, ratings, validation) |
| `run_simulation.R` | Config + end-to-end pipeline |
| `run_backtest.R` | Stage-5 driver (`Rscript run_backtest.R`) |
| `tests/testthat/` | Correctness checks for every module |

## Key knobs (`run_simulation.R` → `config`)

- `xi` — time-decay per day. 0.0018 ≈ Dixon-Coles' original; higher = more weight on recent form.
- `ridge` — shrinks attack/defence toward league average; matters most for promoted teams with few matches.
- `n_sims` — 10,000 runs in ~2 s; 100,000 in ~20 s.
- `max_goals` — score-matrix truncation (10 is safe; P(>10 goals) ≈ 0).
- `param_draws` — rating vectors sampled from the fit's posterior; 1 = point estimate. 500 (default) makes low-data teams' forecasts appropriately wide at ~15 s per run.
