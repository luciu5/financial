# Pre-migration behavioral oracles

Captured per `design/ARCHITECTURE.md` "Oracle strategy" (Phase 5, before migration).

**Rule of use**: migrated `financial` code must be tested AGAINST THESE FIXTURES,
never against itself. These fixtures were captured from the legacy repos at the
frozen SHAs below, BEFORE any migration/refactoring of the economics.

## Source SHAs (matches ARCHITECTURE.md)

| Repo | Path | SHA | Verified |
|---|---|---|---|
| mergersdebt | `~/Projects/completed/MergerDebt` | `ea727430509851c6fe972dbc0a8a56935e54dac7` | `git log -1 --format='%H'` in that repo, 2024-04-01 |
| stability | `~/Projects/active/stability` | `cc95e2e07c45cfca1d0f69f14a655b8c01fb1614` | `git log -1 --format='%H'` in that repo, 2026-07-16 |

Both repos had only harmless untracked scratch files (`.Rproj`, `.Rhistory`,
ad hoc scripts) at capture time — no tracked source was modified. Repos were
NOT modified by this capture process (read-only devtools::load_all / source()).

## Environment

- R version: `R version 4.4.1 (2024-06-14)`
- `antitrust` (installed, CRAN/local lib): version `0.99.35`
- `BB`: `2026.1.0`
- `cubature`: `2.1.4.1`
- `dplyr`: `1.2.1`
- MergerDebt loaded via `devtools::load_all()` (pkgload), NOT installed as a package.
- stability's `counterfactual_solver.R` sourced directly with base `source()` —
  it is pure R with no DB/FRED dependency, confirmed by successfully sourcing
  it standalone (see "Missing-object check" below).

## Task 1 — Debt module (mergersdebt)

Script: `capture_debt_oracles.R`. Run via:

```
cd /fsr/home/m1cst00/Projects/ioverse/financial
Rscript tests/oracles/capture_debt_oracles.R
```

Loads legacy package with:
```r
library(antitrust)
devtools::load_all("/fsr/home/m1cst00/Projects/completed/MergerDebt")
```
This loaded cleanly with no errors (antitrust 0.99.35 satisfies the
`Depends: antitrust` requirement).

**Documented example bug confirmed and corrected**: `R/DebtFunctions.R:59-82`'s
`@examples` block sets `mktSize <- 1` for a 2-market example; `logit.debt()`
requires `length(mktSize) == nMarkets == 2`. All fixtures below use the
corrected `mktSize <- c(1,1)`.

Fixed seed `42` used for every case (before each `logit.debt()`/`logit()` call,
via `set.seed(42)`) even though, empirically, no path exercised here appears
to rely on RNG (no randomness observed in `calcSlopes`/`calcPrices`/`calcShares`
for these inputs) — set defensively per the task's "use a FIXED seed where any
randomness exists" instruction.

### Fixtures

- **`debt_goldenmaster_grid.rds`** / **`debt_goldenmaster_grid_manifest.csv`**
  Grid of `logit.debt()` runs on the corrected 3-firm/2-market example
  (firms 1&2 merge), crossing debt level (`debt_zero`, `debt_low` = doc
  example's `c(22.69,NA,51.002,NA,2.922,NA)`, `debt_high` =
  `c(60,NA,90,NA,40,NA)`) × density (`"uniform"` with `shareOutParm=c(1,1)`,
  `"beta"` with calibrated `shareOutParm`). 6 cases total, keyed
  `zero_uniform`, `low_uniform`, `high_uniform`, `zero_beta`, `low_beta`,
  `high_beta`. Each case stores: full inputs, `pricePre`, `pricePost`,
  calibrated `slopes` (`alpha`, `meanval`), calibrated/fixed `shareOutParm`,
  `sharesPre`/`sharesPost` (via `calcShares`), `mcPre`/`mcPost`, and the
  `summary(fit, market=TRUE)` data.frame (HHI, HHI-debt, paasche index,
  leverage, isProfitable columns).
  **Note**: the `zero_uniform` case triggers a `BBsolve` non-convergence
  warning ("Lack of improvement in objective function") — this is the
  legacy solver's actual behavior at debt=0/uniform and is captured as-is
  (do not "fix" it when reproducing against this fixture; the migrated
  solver may legitimately behave differently and that divergence should be
  reviewed, not silently matched).

- **`debt_zerodebt_direction_sanity.rds`** / **`.csv`**
  Zero-debt run of `logit.debt(debtPre=debtPost=0, density="uniform",
  shareOutParm=c(1,1))` vs. the analogous `antitrust::logit()` merger, on a
  single-market 3-single-product-firm example (firms 1&2 merge, firm 3
  rival). **This is a SIGN/DIRECTION check only** — `logit.debt()` at debt=0
  still runs its own Beta/uniform-outside-share calibration machinery
  (`calcSlopes`/`calcShares` with the `s0Focal`/`hFocalIntegral`/
  `gFocalIntegral` apparatus), which is NOT algebraically identical to plain
  `antitrust::logit()`'s calibration even when debt is zero. Do NOT assert
  exact numeric equality against this fixture; assert only that both give
  price INCREASES for all three products post-merger (captured result:
  `logit.debt` pct changes ≈ `[18.43, 11.65, 3.48]`%; `antitrust::logit` pct
  changes ≈ `[32.34, 20.24, 11.00]`% — same sign/order, different magnitude).

- **`debt_singlemarket_closedform.rds`** / **`.csv`**
  Single-market (`nMarkets==1`) case with 3 single-product firms and modest
  debt (`c(0.5, 0.6, 0.4)`, chosen so `s0Focal` lands inside `(0,1)` and is
  hand-verifiable — the doc example's debt is too large relative to profit
  for this smaller-profit synthetic example and produces `s0Focal` outside
  `[0,1]`, which the internal code silently clips; see caveat below). Stores
  calibrated `alpha`/`meanval`, `pricePre`, `mcPre`, conditional shares
  `sCond`, `profitsCond` (= `mktSize * (pricePre-mcPre) * sCond`, aggregated
  to firm level), and both the manually-computed closed form
  `s0Focal = 1 - debt/profitsCond` and the package's own
  `calcShares(preMerger=TRUE, outSideOnly=TRUE)` result for comparison.
  **Caveat found while probing**: with the doc example's debt levels
  (`c(2,3,1)` on this smaller synthetic profit base), `1 - debt/profit` goes
  NEGATIVE for two firms, and the package's internal `shareOutFocal` clips to
  `[0,1]` before returning (`hFun/gFun` ratio can produce `NaN`/`0/0` in that
  regime) — this is documented in-code behavior, not a bug introduced by this
  capture. The fixture uses debt levels where the closed form stays inside
  `(0,1)` so the hand-computation is directly comparable without clipping
  artifacts; the clipping behavior itself should be captured separately if
  the migration needs to preserve it exactly.

## Task 2 — Bank module (stability) — the strongest oracle

Script: `capture_bank_oracles.R`. Run via:

```
cd /fsr/home/m1cst00/Projects/ioverse/financial
Rscript tests/oracles/capture_bank_oracles.R
```

Sources ONLY:
```r
source("/fsr/home/m1cst00/Projects/active/stability/code/counterfactual_solver.R")
```

**Missing-object check**: sourcing `counterfactual_solver.R` alone (without
`code/lib/merger_common.R`) succeeds with NO missing-object errors. All
functions needed for this task — `calc_portfolio_liquidity`,
`simulate_beta_portfolio_liquidity`, `beta_tail_prob`,
`beta_tail_first_moment`, `beta_tail_second_moment`, `beta_lower_prob`,
`beta_lower_first_moment`, `beta_lower_second_moment`, `beta_inside_shapes`,
`beta_inside_mean`, `beta_inside_var`, `normal_excess_second_moment`,
`single_market_beta_excess_moments` — are self-contained in
`counterfactual_solver.R` and did not require `merger_common.R` (which itself
only re-sources `counterfactual_solver.R` plus Stan-draw-IO helpers that are
out of migration scope per ARCHITECTURE.md). `%||%` (used internally) is
resolved from base R 4.4.1's built-in null-coalescing operator — confirmed
present (`R.version` >= 4.4.0), no external package needed.

Fixed seed `20260910` used throughout (`set.seed()` before `runif()` input
generation, and `seed=` argument passed directly into
`simulate_beta_portfolio_liquidity()` for the Monte-Carlo draws).

### Fixtures

- **`bank_liquidity_mc_vs_cf.rds`** / **`.csv`** — PRIMARY REGRESSION BASELINE.
  Synthetic 5-market bank portfolio (`market_size_L`/`D`, `s_cond_L`/`D`,
  `outside_mu_L`/`D` ~ `runif(0.3,0.7)`, `phi_out_L`/`D` ~ `runif(5,30)`,
  `equity=10`, `liquid_assets=5`, `psi=2`, seed `20260910`). Computes:
  - `calc_portfolio_liquidity(..., method="normal")` (closed-form Normal
    approximation, forced even though 5 markets would otherwise default to
    the same Normal branch since `method="hybrid"` only diverges to the
    exact branch for exactly 1 market)
  - `simulate_beta_portfolio_liquidity(..., draws=200000, seed=20260910)`
    (Monte-Carlo)
  **Exact numeric result of the check**:
  closed-form `expected_liq_penalty = 21.55035`,
  Monte-Carlo `expected_liq_penalty = 21.05315`
  (absolute diff `0.4972`, relative diff `2.31%`).
  closed-form `lambda_liq = 0.5412`, MC `lambda_liq = 2.8109` (these are NOT
  expected to match — `lambda_liq` from the Normal closed form is a smooth
  tail-probability-weighted marginal shadow price, whereas the MC
  `lambda_liq` returned by `simulate_beta_portfolio_liquidity` is
  `psi * mean(excess)`, a different quantity in the same units but not
  algebraically identical; only `expected_liq_penalty` is the shared
  closed-form/MC comparison point).
  **Repeat-seed sanity** (not saved as a fixture, done only to characterize
  noise while building this oracle): re-running the MC at seeds 1-5 gave
  `expected_liq_penalty` in `[21.12, 21.32]`, i.e. relative diffs of
  `1.0%-2.0%` from the closed form — confirming the `2.31%` gap at seed
  `20260910` is ordinary Monte-Carlo sampling noise at `draws=200000`, not a
  systematic discrepancy between the two implementations.

- **`bank_limiting_phi_large.rds`** — limiting case (a): `phi_out_L`/`D` set to
  `1e8` (near-zero variance) on the same 5-market portfolio. Result:
  `mu_W = -12.49082`, `sigma_W = 0.006565` (collapsed near the variance
  floor), `expected_liq_penalty = 0`, and the deterministic-shortfall formula
  `0.5*psi*max(mu_W - liquid_assets, 0)^2 = 0` (since `mu_W < liquid_assets`
  here, both are exactly 0; abs diff `0`). Stores full inputs, the
  `calc_portfolio_liquidity` result, and the manually-computed deterministic
  formula for comparison.

- **`bank_limiting_psi_zero.rds`** — limiting case (b): `psi=0` on the same
  portfolio. Result: `expected_liq_penalty = 0` and `lambda_liq = 0` exactly,
  confirming the friction parameter linearly scales the liquidity cost to
  zero regardless of `mu_W`/`sigma_W`.

- **`bank_limiting_single_vs_multi_market.rds`** / (values also folded into
  `bank_limiting_cases_manifest.csv`) — limiting case (c): the same 5-market
  portfolio's exposures collapsed into ONE market (`market_id = rep(1,5)`),
  compared three ways:
  - `method="hybrid"` on the collapsed single market → dispatches to the
    EXACT `single_market_beta_excess_moments()` branch
    (`liquidity_method == "single_market_beta_exact"`),
    `expected_liq_penalty = 182.909`
  - `method="normal"` forced on the SAME collapsed single-market inputs (an
    apples-to-apples exact-vs-approx comparison), `expected_liq_penalty =
    240.931`
  - `method="normal"` on the ORIGINAL 5 separate markets (== the primary
    oracle's closed-form result), `expected_liq_penalty = 21.550`
  The large gap between the single-market exact/normal figures and the
  genuinely-multi-market figure is expected and economically meaningful: it
  reflects diversification (variance of a sum of 5 quasi-independent market
  exposures is much smaller than variance if all 5 exposures are treated as
  one un-diversified pool) — a real feature of the model, not a bug. The
  gap between EXACT (182.9) and NORMAL-forced (240.9) on the *same*
  single-market inputs is the actual Beta-vs-Normal approximation error this
  limiting case is meant to characterize.

- **`beta_moments_grid.rds`** / **`.csv`** — the shared Beta-moment primitive
  grid. `expand.grid(cutoff = c(-0.5,0,0.001,0.1,0.25,0.5,0.75,0.9,0.999,1,1.5),
  shape1 = c(0.5,1,2,4,10), shape2 = c(0.5,1,2,4,10))` = 275 rows, with columns
  for all six functions: `beta_tail_prob`, `beta_tail_first_moment`,
  `beta_tail_second_moment`, `beta_lower_prob`, `beta_lower_first_moment`,
  `beta_lower_second_moment`. Includes boundary/out-of-range cutoffs
  (`-0.5`, `1.5`) to exercise the `ifelse(cutoff<=0, ...)` /
  `ifelse(cutoff>=1, ...)` branches explicitly coded in
  `counterfactual_solver.R` lines 49-75. No non-finite values found across
  the full grid. **This is the primitive that `shared-private/beta-moments.R`
  must reproduce exactly** (per ARCHITECTURE.md's "shared-private/" section):
  note the migrated debt-module analogs (`hFocalIntegral`/`gFocalIntegral`)
  use `pbeta(u, shape1+1, shape2+1)` (both shapes shifted) for the "h"
  integral and `pbeta(u, shape1, shape2+1)` for the "g" integral, structurally
  parallel to but NOT the same shift pattern as `beta_tail_first_moment`
  (shifts `shape1` only) — confirm which of these (if any) the migrated
  primitive is meant to unify before assuming a 1:1 correspondence.

- **`beta_inside_shapes_grid.rds`** / **`.csv`** — supporting grid over
  `beta_inside_mean`, `beta_inside_var`, `beta_inside_shapes` (used to convert
  `outside_mu`/`phi` parameterization into Beta `shape1`/`shape2` inputs for
  the primitives above), over `mu = c(0.05,0.2,0.5,0.8,0.95)` ×
  `phi = c(1,5,20,100,1000)` (25 rows).

## Reproducing

Both capture scripts are idempotent and self-contained (absolute paths, fixed
seeds). Re-running either script overwrites its own fixtures only; nothing in
either script writes into the source repos. To regenerate everything:

```
cd /fsr/home/m1cst00/Projects/ioverse/financial
Rscript tests/oracles/capture_debt_oracles.R
Rscript tests/oracles/capture_bank_oracles.R
```

## Known blockers/deviations from the task spec

- None outright blocking. The two caveats worth flagging to the architecture
  lead:
  1. Debt single-market closed form: the doc example's debt levels push
     `s0Focal` outside `[0,1]` on a small synthetic profit base, exercising
     the package's internal clipping — captured with smaller debt so the
     hand-computed closed form is directly comparable without clipping; the
     clipping behavior itself is documented but not separately fixtured.
  2. Bank MC-vs-CF `lambda_liq` fields are NOT the same quantity between the
     two implementations (see `bank_liquidity_mc_vs_cf` notes above) — only
     `expected_liq_penalty` is the apples-to-apples comparison point.
