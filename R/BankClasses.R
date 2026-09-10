#' Bank liquidity / stability class definitions (stub)
#'
#' This file is the ownership placeholder for the bank module's S4 class
#' hierarchy, migrated from the `stability` analysis project. Per
#' `design/ARCHITECTURE.md`, it will define (in a later implementation
#' wave, NOT in this skeleton pass):
#'
#' - `BankFit`, `contains = "FinancialFit"` -- the concrete bank-family
#'   structural fit. Takes fitted liquidity/friction parameters as input
#'   (scope decision: "solver + primitives only"); no Stan estimation and
#'   no DB/FRED/QCEW ingestion are migrated into this package.
#' - A bank pricing object, `contains = "antitrust::Antitrust"`, to inherit
#'   the ownership-matrix slot contract and dispatch on `ownerToMatrix()`/
#'   `ownerToVec()` instead of redeclaring it. (Requires antitrust to
#'   export the `Antitrust` virtual base class -- the one required
#'   antitrust extension point noted in the architecture contract.)
#' - Financial state: liquidity-stress `W̃`, liquid-asset buffer,
#'   friction `ψ`, priced as an endogenous marginal-cost shifter
#'   `Λ` (convex quadratic penalty). No regulatory constraint --
#'   liquidity risk IS the stability mechanism; no capital/LCR/NSFR.
#'
#' No class definitions are provided yet. This file exists solely so that
#' the `Collate:` ordering and file ownership are fixed before economics
#' land.
#'
#' @name Bank-classes
#' @keywords internal
NULL
