# CLAUDE.md

R project: EPL match prediction (Dixon-Coles) + Monte Carlo season simulation.
Read `PLAN.md` first for status and next steps; `README.md` for usage.

## Commands
- Run pipeline (synthetic, no network): `USE_SYNTHETIC=1 Rscript run_simulation.R`
- Run pipeline (real data): `Rscript run_simulation.R`
- Tests: `Rscript -e 'testthat::test_dir("tests/testthat")'` — must pass before any commit.
- Pipeline writes a Quarto report to `output/reports/`; `SKIP_REPORT=1` skips it (e.g. when iterating on the model). Report template is `report.qmd`; it reads only `output/simulation.rds` and files in `output/`, never refits.
- Backtest (Stage 5): `Rscript run_backtest.R` (slow, ~10 min; `BACKTEST_STEP=14` for a quick pass).

## Conventions
- Language is R only. Tidyverse style; base R for the numeric core (no new heavy deps without a reason in PLAN.md).
- Canonical results schema is defined at the top of `R/01_load_data.R`; every module consumes it. Don't add columns downstream.
- Matrices from `run_monte_carlo()` are `[n_teams x n_sims]`, rows in `base_table$team` order. Keep it that way.
- `defence` parameters are log-multipliers on goals *conceded*: lower = better.
- No per-simulation loops in the Monte Carlo; keep it matrix algebra.
- Every new feature gets a test in `tests/testthat/`.
- Never commit `data/raw/` or `output/` (gitignored).
