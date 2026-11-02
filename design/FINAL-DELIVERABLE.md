# `financial` package — final deliverable

Migration of two legacy model families (`mergersdebt`, `stability`) into a new `antitrust`
sibling package, per the frozen architecture in `design/ARCHITECTURE.md`.

## 1. Source-repository SHAs inspected

| Repo | Path | SHA |
|---|---|---|
| mergersdebt | `~/Projects/completed/MergerDebt` | `ea727430509851c6fe972dbc0a8a56935e54dac7` |
| stability | `~/Projects/active/stability` | `cc95e2e07c45cfca1d0f69f14a655b8c01fb1614` |

## 2. antitrust SHA used

`0c32b9826f31eed380674976972319487d072833` (refactor branch). This is `24caac92a6c8ad4dc1d3d754c4c26a...`
(the original audit target) **plus one commit** made during this migration: "Export Antitrust virtual
base class for sibling reuse" — the single approved antitrust-side change (§7). Pinned in
`ci/PINNED_SHAS`; installed in an isolated project-local library (`.Rlib/`), not the global
antitrust 0.99.35 (master), which lacks the refactor API entirely.

## 3. Economic model inventory

**Debt module** (`LogitDebt`/`DebtFit`, from mergersdebt): aggregate-Logit Bertrand pricing with
firm-level debt; limited liability implemented in the demand/share layer via a default threshold
`s0Focal = 1 - debt/profit` (single-market) generalized across markets; uncertainty is a
post-price-choice, pre-realization Beta/Uniform-distributed outside share. Calibration:
`optim(L-BFGS-B)` Beta-moment matching; equilibrium: `BB::BBsolve`.

**Bank module** (`BankMarket`/`BankFit`, from stability): Bertrand-Nash-in-rates pricing over
loan/deposit rates with a two-part demand system (conditional logit choice + Beta-distributed
participation/outside share) and an endogenous liquidity-cost marginal-cost shifter (`Lambda`,
from a shortfall distribution over the pooled Beta participation shocks). Merger changes ownership
internalization **and** pools the combined entity's liquidity distribution. Calibration: ingests
already-fitted Stan-posterior parameters (no in-package Stan estimation); equilibrium: damped
fixed-point with `nleqslv` Broyden fallback, generalized to arbitrary ownership-group blocks.

Full per-model attribute table (demand family, conduct, state variables, oracle availability, etc.)
is in the Phase-2 control table produced during the session; the durable summary lives in
`design/ARCHITECTURE.md`.

## 4. Proposed (built) financial package architecture

```
antitrust::StructuralFit (virtual, exported)
        |
   FinancialFit (virtual, in financial)  -- default-reject simulate/simulate_steps/respecify
     |        |
  DebtFit   BankFit   -- WRAP the legacy model in @model (per antitrust's own AntitrustFit
                          precedent), do NOT multiply-inherit the legacy demand hierarchy

  LogitDebt contains antitrust::Logit (unchanged single inheritance)
  BankMarket contains antitrust::Antitrust (ownership reuse)
```

R/ is flat (R packages disallow subdirectories); grouped by filename prefix: `Financial*`,
`shared-beta-moments.R`, `Debt*.R`, `Bank*.R`. Two disjoint economic modules, no shared class
hierarchy below `FinancialFit`.

## 5. Exact shared infrastructure justified by observed duplication

**One module**: `R/shared-beta-moments.R` — partial/incomplete-Beta-moment helpers
(`beta_tail_*`, `beta_lower_*`, `beta_h_focal_integral`, `beta_g_focal_integral`). Verified genuine
duplication: mergersdebt's `hFocalIntegral`/`gFocalIntegral` and stability's
`beta_lower_first_moment`/`beta_tail_*` are the same `pbeta`-ratio partial-moment primitive with
different economic uses (default threshold vs. liquidity shortfall). Validated against the
pre-migration 275-row oracle grid to **0 absolute difference** on all 6 moment functions.
No `FinancialState`/`BalanceSheetState` superclass was created — none was justified by duplication.

## 6. Code migrated versus intentionally left behind

**Migrated**: debt demand/share/pricing/calibration methods (parity); bank deterministic
counterfactual/merger solver and liquidity primitives (parity, generalized to N-bank ownership
groups); shared Beta-moment helpers.

**Left behind (by design)**: stability's Stan estimation model and DB/FRED/QCEW ingestion pipeline
(institution-bound, not portable — `financial` consumes Stan posterior output, does not produce
it); confirmed-dead legacy generic-IO code (`calc_flat_logit_markups`, `make_ownership`,
`merge_owners` — no callers); the exposure-threshold bank/market-selection search (caller now
supplies `reoptimizing` directly).

## 7. Public antitrust APIs consumed

`StructuralFit` (extended), `Antitrust` (extended by `BankMarket`; **exported as part of this
migration** — see §8), `simulate`/`simulate_steps`/`respecify` (S4 generics, methods defined),
`ownerToMatrix`/`ownerToVec`, `HHI`, and the `Logit`/`Bertrand`/`Antitrust` class hierarchy
(via `LogitDebt contains Logit`). NOT used: antitrust's `calibrate()`/`specify()` (closed internal
model registry — `financial` ships its own `debt_fit()`/`bank_fit()` constructors) or any
`antitrust:::` internal (verified zero occurrences).

## 8. Minimal antitrust extension made

One commit to antitrust's refactor branch: exporting the previously-internal `Antitrust` virtual
base class (`exportClasses(Antitrust)`), so `BankMarket` can `contains = "Antitrust"` and reuse
`ownerToMatrix`/`ownerToVec` by inheritance rather than redeclaring an equivalent class. Verified:
NAMESPACE diff is exactly one line; the change is otherwise inert (existing antitrust classes/tests
unaffected — `Antitrust` was already the base of every exported concrete class).

## 9. Parity/oracle results

- Shared Beta helpers: 275/275 rows exact (0 abs diff) against the captured pre-migration oracle.
- Debt: 6/6 golden-master grid cases within 1e-6 (1e-4 for one documented BBsolve
  non-convergence case); single-market closed form (`s0Focal`) exact; zero-debt directional
  sanity vs `antitrust::logit()` confirmed (direction only, by design).
- Bank: closed-form liquidity penalty exact to 1e-8 against its own captured value; agrees with
  an independent Monte-Carlo estimator within the documented ~2.3% noise band; all three limiting
  cases (`phi`→large, `psi`=0, single- vs multi-market) match.
- Full test suite: **136 pass / 0 fail / 0 warn / 0 skip**, verified independently (not just agent
  claims) from a clean `pkgload::load_all(".", reset = TRUE)`.
- Block-diagonal N-bank FOC generalization: independently verified equal to the full
  ownership-system solve across 2+1, dense 3-way, all-standalone, **and** chain-connected
  3-bank topologies (the case a red-team review found the first implementation got wrong —
  see §13).

## 10. CI results

`ci/build-check.sh` + `ci/PINNED_SHAS`: installs the pinned antitrust SHA into an isolated
library, verifies the contract surface (classes + generics the package depends on), builds
`financial`, and runs `R CMD check`. Executed end-to-end from scratch: **Status: OK** (0
WARNING / 0 NOTE / 0 ERROR), "CI PASSED against pinned antitrust 0c32b982...".

## 11. Blockers

None outstanding. (One blocker arose and was resolved mid-migration: the globally-installed
antitrust was master 0.99.35, missing the entire refactor API — resolved by installing the pinned
refactor SHA into a project-local library, documented in `design/DEPENDENCY-ENV.md`.)

## 12. Nonblockers

- Sandbox/environment artifacts (device-node dotfiles, `etc/`/`opt/`) transiently appeared in the
  package working directory during the session (harness-level, not package-related); excluded via
  `.Rbuildignore`/`.gitignore`, do not affect the package or its git history.
- roxygen initially corrupted `Collate:` ordering (alphabetized without dependency info); fixed
  with `@include` tags — roxygen now regenerates docs/NAMESPACE-adjacent state safely and
  repeatably.

## 13. Remaining model debt

- **Debt**: `debtPost != debtPre` (leverage-change) and nonzero-`mcDelta` code paths, the
  `isMax = TRUE` Hessian-maximality check, and the `nMarkets >= 3` closed-form uniform-density
  branch are all carried over verbatim from legacy but have **no oracle coverage** at those
  parameter settings (only `nMarkets = 2`, `debtPost == debtPre`, `mcDelta = 0` are exercised by
  the captured golden-master grid). Recommend capturing additional legacy fixtures before relying
  on these paths in production.
- **Bank**: the N-way (3+ bank) `psiRule = "acquirer"/"target"` convention (first/last element of
  an implicitly-ordered group) has no legacy analogue (legacy only ever had exactly 2 merging
  banks) and is a documented judgment call, not a verified-against-truth convention.
- **Both**: the opt-in "primitive-shift" counterfactual step (letting a merger alter the Beta
  uncertainty law itself, not just firms' exposure to it) is designed for (a seam exists) but not
  implemented, per the user's "build both, opt-in" decision — exposure-only is the validated
  default. `simulate_steps()`/`respecify()` for both `DebtFit` and `BankFit` inherit
  `FinancialFit`'s default-reject stubs; sequential multi-merger `CounterfactualPath` resume is
  not yet implemented (fails loudly rather than silently).
- **Bank**: `BankMarket`'s `assets` slot (added during red-team remediation) is optional and falls
  back to a `liquidAssets + equity` proxy with an explicit warning if a caller doesn't supply
  `data$assets` — callers using `psiRule = "asset_weighted"` in production should supply it.

## 14. Package grade

| Dimension | Grade | Basis |
|---|---|---|
| Architecture | **A-** | Clean StructuralFit extension, correct wrap-not-inherit pattern (matches antitrust's own precedent), disjoint modules honored, one narrow antitrust extension (not a redesign). Minor deduction: bank's N-bank generalization needed a real correctness fix post-review, indicating the initial extraction under-tested its own generalization claims. |
| Economic correctness | **B+** | Core economics (LL-in-demand-layer, financial timing, liquidity pooling formula, general parity/aggregation) independently verified clean by adversarial review. One CRITICAL bug (block-solve chain topology) and one MAJOR bug (psiRule asset-weighting) were found and fixed during red-team — real defects that shipped past the extraction agent's own tests, caught only by adversarial review with a constructed counterexample. |
| Testing | **A-** | 136 tests, all economics tested against pre-migration oracles (never self-referential, verified by SHA cross-check), a dedicated regression test for the fixed CRITICAL bug. Deducted for the documented untested legacy branches (§13) inherited verbatim without fresh fixtures. |
| Release readiness | **A** | `R CMD check` Status OK, reproducible pinned-dependency CI verified end-to-end from scratch, clean docs (roxygen + `@include`), no `antitrust:::`, no sibling deps. |

## 15. Recommended next implementation wave

1. Capture the missing legacy fixtures identified in §13 (leverage-change, `mcDelta`,
   `nMarkets >= 3`, `isMax = TRUE`) and extend parity tests to cover them before any production use
   of those paths.
2. Implement `simulate_steps()`/`respecify()` for sequential multi-merger `CounterfactualPath`
   resume (currently default-reject) — the natural next increment given both modules already
   surface single-step uncertainty-delta outputs.
3. Build the opt-in primitive-shift counterfactual step (user-supplied shift to the Beta
   uncertainty law itself) now that the exposure-only default is validated and the seam is in place.
4. A second, independent red-team pass focused specifically on the bank N-bank generalization's
   remaining judgment calls (psiRule acquirer/target convention, `Z_psi`-without-original-panel
   contract) once real multi-bank merger scenarios are available to validate against.
