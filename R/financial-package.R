#' financial: Financial-Structure Extensions for Merger Simulation
#'
#' Two disjoint financial-structure merger model families built on top of
#' the `antitrust` structural-fit contract:
#'
#' - A **debt / limited-liability** module (migrated from `MergerDebt`):
#'   firm-level debt reshapes conditional demand shares through an
#'   outside-share default threshold.
#' - A **bank liquidity / stability** module (migrated from the `stability`
#'   analysis project): an endogenous marginal-cost shifter driven by
#'   liquidity stress and a pooled post-merger liquidity distribution.
#'
#' `financial` depends only on `antitrust`. It does not depend on, or share
#' code with, any other sibling package. It ships its own constructors
#' (`debt_fit()`, `bank_fit()`) and its own small model registry rather than
#' reusing antitrust's internal `calibrate()`/`specify()` registry.
#'
#' See the frozen architecture contract in `design/ARCHITECTURE.md` for the
#' full migration boundary and lifecycle contract.
#'
#' @keywords internal
"_PACKAGE"

## Quiet R CMD check NOTEs for dplyr non-standard-evaluation column names used
## in summary(LogitDebt) (migrated verbatim from MergerDebt's market-level table).
utils::globalVariables(c(
  "ownerPre", "ownerPost", "marketID", "outputPre", "outputPost",
  "profitsPre", "profitsPost", "leverage", "debtPre", "paasche",
  "paascheNum", "paascheDen", "Pre-Merger HHI", "Post-Merger HHI",
  "Pre-Merger Debt HHI", "HHI Change", "HHI Debt Change"
))

## usethis namespace: start
## usethis namespace: end
NULL
