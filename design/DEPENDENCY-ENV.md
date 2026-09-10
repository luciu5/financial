# Dependency environment (pinned)

`financial` targets the **antitrust refactor branch**, NOT the globally-installed master.

| Item | Value |
|---|---|
| antitrust SHA (pinned) | `0c32b9826f31eed380674976972319487d072833` (refactor; adds Antitrust export). Previously 24caac92 before the export commit. |
| antitrust version | 0.99.38 |
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
