# PLAN.md — EPL Prediction Engine & Monte Carlo Simulation

## Handoff (paste this into a fresh session)

R project that fits a time-weighted Dixon-Coles goal model to Premier League
results and simulates the remaining 2026/27 season 10,000 times to give every
team a Title / Top-4 / Top-6 / Relegation probability. Data comes from
football-data.co.uk (`E0.csv` per season); remaining fixtures are derived from
the unplayed pairings. **Stages 1–6a are DONE and the model is frozen:** xi = 0.0018, ridge = 1,
no prior, param_draws = 500. Match-level log-loss 1.002 vs market 0.993.
Every shrinkage variant tested (ridge 3/5/10, promoted prior 10/20/40) made
forecasts worse, so tuning is closed. Known limitation: teams with < 10
effective matches have wide, sometimes implausible-looking tails (flagged
`low_data` in the forecast table); this self-corrects within ~8 rounds.
Stage 7 (weekly Quarto report) is DONE: `Rscript run_simulation.R` now also
writes `output/reports/MW{XX}_{date}.html`. Weekly cadence: rerun after each
matchweek. Open items are optional Stage 6 extensions. Run `USE_SYNTHETIC=1 Rscript run_simulation.R`
and the tests before touching anything.

## Decisions (and why — don't relitigate without new evidence)

| Decision | Chosen | Rejected | Reason |
|---|---|---|---|
| Goal model | Dixon-Coles Poisson | Independent Poisson; Negative Binomial | Independent Poisson mis-prices draws / low scores; NB adds overdispersion football goals barely show. DC fixes the real defect (0-0 / 1-1 dependence) with one parameter. NB is an optional later switch. |
| Home advantage | Fitted MLE parameter | Fixed multiplier (e.g. 1.3) | Home edge has drifted since 2020; let the data set it. |
| Recent form | Exponential decay `exp(-xi*days)` inside the likelihood | Separate "last-5 form" adjustment | Single principled knob; no double-counting; form effect is tested via xi. |
| MC sampling | Sample cells of the exact DC score matrix per fixture | `rpois` + rho correction; rejection sampling | Exact, vectorised, and reuses the same matrix used for analytic probabilities. |
| Aggregation | Incidence-matrix multiplication | Loop over sims / `group_by` | 10k × 340 fixtures in ~2 s; no parallelism needed. |
| Remaining fixtures | Derived from unplayed pairings | Scrape a fixture list | Zero dependencies; dates are irrelevant because ratings are frozen. Optional loader for fixturedownload.com exists if dates are wanted. |
| Data source | football-data.co.uk | FBref / Understat | Stable schema, no key, 30 seasons of history, includes odds for later calibration. xG can be added via `worldfootballR` later. |

## Stages

### Stage 1 — Walking skeleton  ✅ DONE
Synthetic data → fit → simulate → forecast table printed.
- Verify: `USE_SYNTHETIC=1 Rscript run_simulation.R` prints a 20-row forecast with probabilities summing sensibly.

### Stage 2 — Correctness  ✅ DONE
- Verify: `Rscript -e 'testthat::test_dir("tests/testthat")'` → all pass. Covers: score matrix validity, rho direction, parameter recovery on synthetic truth, sampled vs analytic H/D/A, tie-break order, MC internal consistency.

### Stage 3 — Outputs & plots  ✅ DONE
- Verify: `output/` contains 3 CSVs, 4 PNGs, `simulation.rds`; heatmap rows are ordered by expected position.

### Stage 4 — Real data run  ✅ DONE (19 Sep 2026)
Findings: 25 teams / 800 matches in fit (410 effective). Established sides rated sensibly. Promoted sides not: Hull defence_mult 0.66 after 4 games, Coventry 15 expected points. → tune `ridge` in Stage 5.
- Goal: run the pipeline on 2024/25, 2025/26 and 2026/27 results.
- Where: `run_simulation.R` (config only; no code changes expected).
- Steps:
  1. `Rscript run_simulation.R`. Tripwire: if the download 404s, check the season code on football-data.co.uk/englandm.php and that `E0.csv` is still the file name.
  2. Confirm `derive_remaining_fixtures()` reports `<played> + <remaining> = 380` and 20 teams. If not 20, a promoted team hasn't played or a name changed — inspect `unique(current$home)`.
  3. Sanity-check `strength_table()`: last season's champions near the top, promoted sides near the bottom, home advantage ~×1.2–1.35, rho ≈ -0.03 to -0.15.
  4. Compare `fixture_probabilities.csv` for the next round against bookmaker implied probabilities (columns `B365H/B365D/B365A` in the same CSVs, de-vigged). Differences of a few points are fine; a systematic 10+ point gap means a bug or a bad `xi`.
- Verify: forecast CSV exists, `p_title` sums to ~100 across teams, no warnings from `optim`.
- Fence: do not add xG, odds blending, or new plots in this stage.

### Stage 5 — Calibration & tuning  ✅ DONE
**Round 1 result (19 Sep 2026, 755 matches, 2024/25–2025/26):** DC log-loss 1.004 vs bookmaker 0.991 vs naive 1.084. xi is flat in {0, 0.001, 0.0018, 0.003} (differences ≪ SE 0.017); xi = 0.005 slightly worse. Global ridge = 10 is clearly worse (+0.02) — it shrinks well-measured teams as hard as promoted ones. **Decision: production stays at xi = 0.0018, ridge = 1.**
Breakdown: the gap to the market is 0.020 on Nov–May matches between established teams (n = 414, the bulk of the deficit), ~0 on promoted-team matches after October, and 0.023 on Aug–Oct promoted-team matches (n = 45, noisy). Promoted-team mis-rating barely affects one-week forecasts but compounds over a 34-fixture season simulation, so it is fixed with a targeted prior rather than global ridge.
**Round 2 result (19 Sep 2026):** pseudo-match prior (`prior_matches` ∈ {0, 10, 20, 40}) is worse at every strength, overall (1.002 → 1.003 → 1.008 → 1.020) AND on early-season promoted-team matches (0.906 → 0.934 → 0.964 → 0.988, where prior=0 beats the bookmaker's 0.945). A fixed promoted-team profile is wrong in both directions: 2024/25's promoted sides were much worse than it, 2025/26's better. **Decision: prior_matches = 0 in production. The argument stays in `fit_dixon_coles()` (default off) for future experiments, e.g. with a prior mean estimated from Championship data.**
**Final production settings: xi = 0.0018, ridge = 1, prior_matches = 0.** Overall log-loss 1.002 vs market 0.993 vs naive 1.092.
**Reframing of the Hull/Coventry issue:** the ratings themselves are as good as the market's after 4 games. The problem is that `run_monte_carlo()` uses point-estimate ratings, so 4-match teams are simulated with the same certainty as 38-match teams → over-confident tails ("Coventry 100%"). Fix = Stage 6a below.
- Goal: choose `xi` and `ridge` by out-of-sample log-loss / Brier on 2024/25 + 2025/26.
- Where: `R/07_backtest.R` (functions), `run_backtest.R` (driver). `fit_dixon_coles()` interface untouched.
- Steps:
  1. `Rscript run_backtest.R` (~80 origins x 10 settings ≈ 800 fits, 10–15 min; `BACKTEST_STEP=14` halves it). Tripwire: odds coverage printed at the top should be ~100% from `pinnacle_closing`; if it falls back to opening odds the bookmaker bar is weaker.
  2. Read `output/backtest_summary.csv`. Pick the (xi, ridge) with lowest log-loss; if two are within one `log_loss_se`, prefer the simpler (lower ridge, xi closer to 0.0018).
  3. Check the by-season table: the winner should not flip between seasons.
  4. Copy the chosen values into `config` in `run_simulation.R`, rerun Stage 4, confirm Hull/Coventry now look plausible.
- Verify: best DC row within ~0.02 log-loss of the bookmaker row and clearly below naive; tests pass.
- Fence: only additive, default-off arguments to `fit_dixon_coles()`; no new model families here (Stage 6).

### Stage 6a — Parameter uncertainty in the simulation  ✅ DONE
Real-data result (19 Sep 2026): Coventry relegation 100% → 93% (points 5–95%: 8–24 → 4–39); Hull points 50–73 → 35–87; Arsenal/City title 61/37 → 52/33. Remaining artefact: Hull title 9.4% — the posterior is symmetric around a flattering 4-match mean.
**Round 3 backtest (ridge ∈ {1, 3, 5}):** monotonically worse again, overall (1.002 → 1.006 → 1.010) and on early-season promoted matches (0.906 → 0.927 → 0.944). **Decision — FINAL: no shrinkage of any kind. Three experiments (ridge 1/3/5/10, prior 10/20/40) all degrade match-level forecasts, and season-level tails cannot be backtested on two seasons. The Hull-type artefact is a documented limitation that self-corrects as data accumulates; the forecast table now carries `eff_matches` and a `low_data` flag so readers see it.** Do not reopen without a new idea that the match-level backtest can validate (e.g. a prior mean estimated from Championship results).
Built: `fit_dixon_coles(hessian = TRUE)` returns `par` and `vcov` (inverse observed information; backtest passes `hessian = FALSE`); `sample_ratings(fit, k)`, `rating_uncertainty(fit)`; `run_monte_carlo(param_draws = K)` simulates K blocks of seasons each under its own posterior draw. `score_matrix()` now floors negative Dixon-Coles cells (extreme sampled rho) at zero. Production config: `param_draws = 500`.
Synthetic check: points SD for 3–5-match teams 7 → 16–21 vs 7 → 9–10 for 30+-match teams; a 75% "finish last" became 45%; well-measured teams' expected points move < 1.
- Goal: draw a fresh rating vector per simulated season (or per block of seasons) from the fit's approximate posterior, so uncertainty is widest for low-data teams.
- Where: `fit_dixon_coles()` gains `hessian = TRUE` → store `vcov` (inverse Hessian in the free-parameter space); new `sample_ratings(fit, k)`; `run_monte_carlo()` gains `param_draws = K` (default 1 = current behaviour). `sample_scores()` is called once per draw with n_sims / K seasons.
- Verify: with `param_draws = 1` output is identical to today (regression test); with K = 100, forecast spreads widen most for the teams with the fewest effective matches; Coventry relegation drops well below 100%; overall expected points barely move. Backtest log-loss must not get worse (the posterior-mean forecast is what's scored, so it shouldn't).
- Fence: no change to point-estimate ratings or the backtest; parameter uncertainty only, no model-form changes.

### Stage 6 — Model extensions (each optional, each its own PR)
- Negative Binomial switch in `score_matrix()` with a fitted dispersion.
- xG-based ratings via `worldfootballR` (fit a log-linear model on xG instead of Poisson on goals, or blend).
- Late-season effects (the largest measured gap to the market, 0.02 on Nov–May established-team matches): motivation / dead-rubber flags, or blend with market odds for the next round only.
- Promoted-team prior mean estimated from history (currently hard-coded attack ×0.85, defence ×1.2); or seed from Championship data.
- Verify each: tests pass + backtest log-loss not worse.

### Stage 7 — Delivery  ✅ DONE
`report.qmd` (Quarto, self-contained HTML) rendered automatically at the end of `run_simulation.R` to `output/reports/MW{XX}_{date}.html` via `R/08_report.R::render_report()`. Sections: executive-summary KPI cards, season forecast table (90% intervals, low-data flags, UCL/relegation dividers), upcoming fixtures with model vs de-vigged market probabilities (from football-data's weekly `fixtures.csv`; falls back to the ten closest remaining fixtures if unavailable), all four plots embedded, team ratings ± posterior SD, backtest table if present, methodology.
- Run: `Rscript run_simulation.R` (needs the Quarto CLI + knitr + kableExtra; `SKIP_REPORT=1` to skip). Reports accumulate one file per run, so `output/reports/` becomes the archive for end-of-season evaluation of the season-level forecasts.
- Verify: `output/reports/MWxx_yyyy-mm-dd.html` exists, opens as a single file, and the KPI cards match `season_forecast.csv`.
- Possible follow-ups: Shiny "what-if" mode; a season-end script that scores the archived reports against the final table.

## Risks & tripwires

| Risk | Early warning | Fallback |
|---|---|---|
| football-data.co.uk changes URL/schema | Stage 4 step 1 fails | Loader is 30 lines; point `load_results()` at any CSV with date/home/away/goals. |
| Promoted teams badly rated after 5 games | Absurd strength values or 40%+ relegation for a mid-table promoted side | Use `prior_matches` (global `ridge` is proven not to work); or add Championship results (`E1.csv`) to the fit. |
| `optim` non-convergence | Warning printed by `fit_dixon_coles()` | Increase `maxit`, tighten `rho` bounds, or start from a Poisson GLM fit (`glm(goals ~ ...)`) as `p0`. |
| Overconfident forecasts | Backtest log-loss worse than bookmakers by > 0.03 | Lower `xi`, add ridge, or blend with market odds. |
