#' Debt / limited-liability class definitions (stub)
#'
#' This file is the ownership placeholder for the debt module's S4 class
#' hierarchy, migrated from `MergerDebt`. Per `design/ARCHITECTURE.md`, it
#' will define (in a later implementation wave, NOT in this skeleton pass):
#'
#' - `LogitDebt`, `contains = c("antitrust::Logit", "DebtFit")` -- reuse by
#'   inheritance/dispatch is the intended pattern; debt enters via
#'   firm-level `debtPre`/`debtPost`, and limited liability lives in the
#'   demand/share layer via the `s0Focal` default-threshold (profit = debt
#'   boundary), reshaping conditional logit shares through partial-Beta
#'   integrals. This is preserved as-is for parity with the legacy
#'   `mergersdebt` implementation and is flagged for red-team economic
#'   review; it is NOT to be rewritten during extraction.
#' - `DebtFit`, `contains = "FinancialFit"` -- the concrete debt-family
#'   structural fit, constructed only via this package's own `debt_fit()`
#'   constructor (antitrust's internal `calibrate()`/`specify()` registry
#'   is not reused).
#'
#' No class definitions are provided yet. This file exists solely so that
#' the `Collate:` ordering and file ownership are fixed before economics
#' land.
#'
#' @name Debt-classes
#' @keywords internal
NULL
