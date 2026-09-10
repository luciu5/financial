# `financial` package — frozen migration architecture

Status: **FROZEN** (Phase 2–4 complete). Implementation waves build against this contract.
Governing rule: AI proposes; inspected repository evidence and executed code decide.

## Source SHAs inspected

| Repo | Path | SHA | Notes |
|---|---|---|---|
| mergersdebt | `~/Projects/completed/MergerDebt` | `ea727430` | real R pkg, `Depends: antitrust` |
| stability | `~/Projects/active/stability` | `cc95e2e0` | flat R/Stan analysis project, NOT a package |
| antitrust (audit target) | refactor branch @ `/tmp/antitrust-refactor` | `24caac92` | public API surface |

## Boundary summary

`financial` depends only on `antitrust`. No sibling→sibling deps. No `antitrust:::`.
No copied demand engines / ownership machinery / counterfactual grammar. No infra package.

## Two disjoint economic modules

### A. Debt module (from mergersdebt)
- Class `LogitDebt` **keeps** `contains = "antitrust::Logit"` (→ Bertrand → Antitrust). Reuse by inheritance/dispatch — the intended pattern.
- Debt enters via firm-level `debtPre`/`debtPost`. Limited liability lives in the **demand/share layer** via the `s0Focal` default-threshold (profit = debt boundary), reshaping conditional logit shares through partial-Beta integrals.
  - ⚠️ Economic-correctness flag: spec Phase-7C prefers LL in the objective/payoff layer. **Preserve as-is for parity; red-team reviews it.** Do NOT rewrite during extraction.
- Uncertainty: post-price-choice, demand-side; outside share `u ~ Beta` (or Uniform). Debt does NOT modify marginal cost (no fake-MC).
- Solver: `BB::BBsolve`. Calibration: `optim(L-BFGS-B)` + Beta moment matching.

### B. Bank module (from stability)
- Migrate **only** the deterministic R counterfactual/merger solver + closed-form Beta/liquidity primitives (`counterfactual_solver.R`, live parts of `merger_common.R`, `simulate_merger_exposure.R`/`simulate_merger_market.R`).
- **Do NOT migrate**: Stan estimation (`stability_model.stan`), DB/FRED/QCEW ingestion (`create_stability_data.R`) — institution-bound, not portable. **Do NOT migrate dead code** (`calc_flat_logit_markups`, `make_ownership`, `merge_owners` — no callers).
- `BankFit` takes fitted parameters as **input** (scope decision: "solver + primitives only").
- **Stan-output ingestion is a hard requirement** (user intent: use Stan posterior output for counterfactual sims). The parameter-ingestion contract must be Stan-posterior-shaped:
  - `bank_fit(parameters = ...)` accepts EITHER a point-estimate parameter vector/list OR a posterior draws object — an `N_draws x N_params` matrix, a `posterior::draws`/cmdstanr `$draws()` object, or a plain data.frame. Do NOT force a cmdstanr dependency (accept a matrix/data.frame; treat posterior/cmdstanr as `Suggests` for optional convenience coercers only).
  - The bank `simulate`/counterfactual method must MAP OVER posterior draws and return posterior summaries (per-quantity draws -> mean/sd/quantiles + posterior probability, e.g. P(liquidity benefit > 0)) — mirroring stability's `run_posterior_merger_driver()` + `gather_merger_results.R`. Reuse antitrust `CounterfactualPath` bookkeeping per draw.
  - This stays within "solver + primitives only": the package CONSUMES Stan output, does not PRODUCE it. Leaves the door open to a later optional in-package Stan backend with no redesign.
- Financial state: liquidity-stress `W̃`, liquid-asset buffer, friction `ψ`, priced as endogenous marginal-cost shifter `Λ` (convex quadratic penalty). No regulatory constraint — liquidity risk IS the stability mechanism. No capital/LCR/NSFR.
- Merger changes ownership internalization **and** pools the combined entity's liquidity distribution.
- Bank calibration strategy: **TBD** (recommendation pending): `specify()` supplies params directly; `calibrate()` via method-of-moments from observed rates/shares/margins + liquidity observables (no Stan in-package).

## Fit hierarchy: WRAP, do not multiply-inherit (RULING)

Decided from antitrust's own precedent (verified in refactor ModelArchitecture.R): `AntitrustFit`
`contains = "StructuralFit"` and holds the legacy S4 model in its `@model` slot (ANY); methods read
`fit@model` (e.g. line 1206 `model <- fit@model`). It does NOT multiply-inherit the legacy hierarchy.

`financial` follows the SAME pattern — WRAPPING, not multiple inheritance:
- `DebtFit contains "FinancialFit"` (-> StructuralFit); holds a `LogitDebt` in `@model`. `LogitDebt`
  keeps `contains "Logit"` UNCHANGED. DebtFit's simulate/respecify/simulate_steps delegate to the
  LogitDebt economics via `@model`. DO NOT make LogitDebt itself a FinancialFit (avoids the
  two-base-hierarchy / multiple-inheritance trap: Logit->Bertrand->Antitrust vs StructuralFit).
- `BankFit contains "FinancialFit"`; holds the bank pricing/state object in `@model`; THAT inner
  object `contains "Antitrust"` for ownerToMatrix/ownerToVec reuse.

## Fit / lifecycle contract

```
antitrust::StructuralFit  (virtual, exported)
        |
   FinancialFit           (virtual, in financial)
     |        |
  DebtFit   BankFit        (disjoint concrete subclasses; separate economics)
```

- `FinancialFit contains = "StructuralFit"`; supplies financial structural state + dispatch only. Subclasses own economics.
- Implement the **extensible S4 generics** with per-subclass methods: `simulate`, `simulate_steps`, `respecify`. Default-reject inherited from StructuralFit means unsupported ops fail loudly.
- `DebtFit`/`BankFit` plug into antitrust's `CounterfactualPath` resume (it dispatches polymorphically through `simulate_steps`).
- **NOT reused**: antitrust `calibrate()`/`specify()` (closed internal registry). `financial` ships its own constructors `debt_fit()`/`bank_fit()` + its own small model registry.
- Reuse antitrust **functions/generics**: `HHI()`, `elast`, `diversion`, `CV`, `calcProducerSurplus`, `upp`, `cmcr.*`, and `ownerToMatrix`/`ownerToVec` (bank pricing object `contains = "Antitrust"`).
- Lifecycle methods implemented only where economically meaningful; unsupported ops error explicitly.

## Counterfactual uncertainty response (user requirement)

Requirement: counterfactuals must report **how additional (sequential) mergers affect the uncertainty faced by firms**. Decision = "build both, primitive-shift opt-in".

- Sequential mergers use antitrust `CounterfactualPath` + `add_step` + `simulate_steps` (already required of DebtFit/BankFit). The path CARRIES UNCERTAINTY STATE FORWARD and each step records the change in firm uncertainty — cumulative across additional mergers.
- **Default path = EXPOSURE-ONLY (fixed shock law), validated, extraction-faithful.** Each merger changes ownership/leverage/footprint; the Beta shock law is unchanged. Reported uncertainty deltas:
  - Bank: Δ Var(pooled shortfall W̃), Δ shadow-cost Λ distribution, Δ E[Γ] (diversification effect; compounds over the path).
  - Debt: Δ P(u ∈ default region), Δ survival-weighted expected profit (from ownership pooling + debtPost).
- **Opt-in path = PRIMITIVE-SHIFT.** `financial` defines its OWN counterfactual step type (antitrust's `CounterfactualStep` is concrete/non-subclassable, so financial needs its own anyway) with an optional `uncertainty` field: a user-supplied merger-induced change to the Beta primitive (bank: participation precision φ / shareOutParm; debt: outside-share law). When absent → identical to exposure-only default (must not perturb parity tests). This is user-supplied input, NOT new estimated economics — stays within "solver + primitives only".
- These uncertainty quantities become FIRST-CLASS counterfactual outputs (not side CSV columns as in legacy stability).

## Required minimal antitrust extension point

- **Export the `Antitrust` virtual base class** (currently internal; `ownerToMatrix`/`ownerToVec` dispatch on it but it is not in NAMESPACE). User approved. This lets bank classes `contains = "Antitrust"` to inherit the ownership slot contract + dispatch instead of redeclaring it. This is the ONLY antitrust change needed for the frozen design.

## shared-private/ — exactly one module

**Partial/incomplete-Beta-moment helpers.** Confirmed genuine duplication (verified in code):
- mergersdebt `hFocalIntegral`/`gFocalIntegral`: `beta(s1+1,s2+1)/beta(s1,s2) * pbeta(u, s1+1, s2+1)`
- stability `beta_lower_first_moment`/`beta_tail_*`: `base * pbeta(cutoff, shape1+1, shape2)`

Both are partial-Beta moments of a share on [0,1] via `pbeta` ratios. The economic USES diverge (default threshold vs liquidity shortfall) — share only the primitive, **NOT** a `FinancialState`/`BalanceSheetState` superclass.

## R/ layout (FLAT — R packages do NOT allow subdirectories under R/)

Grouping is by FILENAME PREFIX, not directories. Verified: `R/shared/…` fails to load.

```
R/
  financial-package.R        # package doc
  FinancialArchitecture.R    # FinancialFit virtual class + default-reject dispatch
  shared-beta-moments.R      # ONLY genuinely shared helper (DONE, oracle-validated)
  FinancialFit.R             # constructors, registry, lifecycle glue

  DebtClasses.R              # LogitDebt (contains antitrust::Logit)
  DebtEquilibrium.R          # calcShares/Slopes/Prices/MC/Margins
  DebtCounterfactual.R       # merger + leverage-change simulate/simulate_steps
  DebtSummary.R              # summary/plotBeta

  BankClasses.R              # BankFit (contains FinancialFit; pricing obj contains Antitrust)
  BankBalanceSheet.R         # liquidity-stress state, buffer, psi
  BankEquilibrium.R          # Nash-in-rates fixed point (nleqslv fallback)
  BankCounterfactual.R       # merger + liquidity pooling; posterior-draw mapping
  BankRisk.R                 # Lambda liquidity cost, shortfall
```

## Oracle strategy (Phase 5, before migration)

- **Bank (strongest):** closed-form `calc_portfolio_liquidity` vs Monte-Carlo `simulate_beta_portfolio_liquidity` — independent implementations, no DB/FRED. Limiting cases: φ→∞, ψ=0, single-market exact vs Normal. Make this the primary regression test.
- **Debt:** fix example (`mktSize=c(1,1)`), golden-master grid (debt × density × markets); zero-debt directional sanity vs `antitrust::logit()`; single-market `s0Focal = 1 - debt/profit` closed form.
- Never test migrated code against itself; capture fixtures from legacy execution BEFORE changing economics.

## Known legacy defects to fix during migration

- mergersdebt: `numDeriv::genD` unimported (isMax path broken); broken example `mktSize`; density prototype/arg mismatch; "Bertran" typo.
- stability: dead generic-IO helpers not migrated.
