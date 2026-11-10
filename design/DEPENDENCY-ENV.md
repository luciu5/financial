# Dependency environment (historical pinned capture)

> Historical development record from September2026, superseded by
> [the current README](../README.md). The retired refactor branch and personal
> library path below are not installation or ordinary-use requirements.

`financial` targets the **antitrust refactor branch**, NOT the globally-installed master.

| Item | Value |
|---|---|
| antitrust SHA (pinned) | `d9788f3949aa08de786bff19aec579d8d3d1cf78` (refactor tip, 2026-09-11; synced past upstream vertical-extraction/BLP work with the Antitrust-export commit cherry-picked forward). Superseded pins: `0c32b982...` (24caac92 + export, before refactor advanced), `24caac92` (before export). |
| antitrust version | 0.99.39 |
| Installed into | `~/Projects/ioverse/financial/.Rlib` (project-local) |
| Global antitrust (untouched) | 0.99.35 (master) in `~/R/x86_64-ubuntu18-linux-gnu-library/4.4` |
| R version | 4.4.1 |
| antitrust deps | methods, BB, numDeriv, nleqslv, SQUAREM (all in base user lib) |

## Correct invocation for ALL financial load/test/check commands

Prepend the project lib IN-SCRIPT (do NOT use `R_LIBS_USER=` env — it REPLACES the user lib and hides nleqslv/BB/SQUAREM):

```r
.libPaths(c("/fsr/home/m1cst00/Projects/ioverse/financial/.Rlib", .libPaths()))
```

Verified: antitrust@refactor exposes StructuralFit, AntitrustFit, CounterfactualPath (exported+defined), and generics simulate/simulate_steps/respecify/calibrate/specify + HHI/elast/CV/calcProducerSurplus/ownerToMatrix/ownerToVec.

`Antitrust` base class: **now EXPORTED** (commit 0c32b982 on refactor) so bank pricing objects can `contains="Antitrust"`. Verified: exported, concrete (non-virtual) base — fine for contains=.

Skeleton verified loading against this lib: FinancialFit is virtual, extends StructuralFit, and has default-reject methods for simulate/simulate_steps/respecify.

## ⚠️ roxygen requires `@include` tags for correct Collate ordering

roxygen re-derives the DESCRIPTION `Collate:` field. WITHOUT dependency info it falls back to
ALPHABETICAL order, which puts DebtClasses.R before FinancialArchitecture.R and breaks load
(DebtFit contains FinancialFit, defined there). This corrupted Collate twice before `@include`
tags were added.

FIX (in place): each R file carries `@include` tags encoding load order, so roxygen computes
the CORRECT topological Collate. Run roxygen WITH the project lib on the path so the
cross-package antitrust::StructuralFit resolves during roxygen's load step:
`.libPaths(c("<financial>/.Rlib", .libPaths())); roxygen2::roxygenise(".")`.

The known-good Collate order (which the @include graph reproduces): financial-package.R ->
FinancialArchitecture.R -> shared-beta-moments.R -> Debt*.R -> Bank*.R.

ALWAYS verify after any roxygen run: (1) Collate matches the known-good order, (2) NAMESPACE
still has all exports, (3) `pkgload::load_all(".", reset=TRUE)` loads (reset=TRUE catches
Collate-order bugs; a warm load masks them).
