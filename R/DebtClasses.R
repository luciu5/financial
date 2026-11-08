#' Debt / limited-liability class definitions
#'
#' S4 classes migrated from `MergerDebt` (`DebtClasses.R`, SHA `ea727430`) to
#' model the effect of firm-level debt on mergers.
#'
#' @section `LogitDebt`:
#' `LogitDebt` **keeps** `contains = "Logit"` (-> `Bertrand` -> `Antitrust`)
#' exactly as in the legacy package -- this is a parity migration of the
#' economics, not a redesign. `LogitDebt` has the information for a Nash
#' Bertrand pricing game with Logit demand and firm-level debt. Code supports
#' firms operating in multiple markets.
#'
#' Let k denote the number of products produced by all firms across all
#' markets. Let f denote the number of firms producing products across all
#' markets. Let m denote the number of markets.
#'
#' Objects are normally created with the legacy-name constructor
#' [logit.debt()], which preserves the legacy signature for parity.
#'
#' @slot marketID A length k character vector identifying which market a
#'   product is sold in.
#' @slot productID A length k character vector identifying a product.
#' @slot debtPre A length k vector of pre-merger PRODUCT-level debt payments
#'   (summed to the firm level internally); NA for products whose firm-level
#'   debt is already captured by a sibling product.
#' @slot debtPost A length k vector of post-merger debt payments. Default is
#'   `debtPre`.
#' @slot density A length-1 character vector, either `"beta"` (default) or
#'   `"uniform"`. If `"beta"`, the calibration routine assumes firm outside
#'   shares are sampled from a [stats::Beta] distribution and uses margin
#'   information to calibrate `shape1`/`shape2`. If `"uniform"`, then
#'   `shape1 = shape2 = 1`.
#'
#'   Legacy defect fixed: the legacy `LogitDebt` prototype default was
#'   `"uniform"` while the legacy `logit.debt()` constructor default was
#'   `"beta"` (via `match.arg(density, c("beta","uniform"))`, which picks the
#'   first alternative). That prototype/constructor mismatch is resolved here
#'   by setting the class prototype default to `"beta"`, matching the
#'   constructor default, per `design/ARCHITECTURE.md`'s migration guidance.
#'   The prototype value is only observed when `new("LogitDebt", ...)` is
#'   called directly without an explicit `density`; the [logit.debt()]
#'   constructor always sets `density` explicitly and is unaffected either
#'   way.
#' @slot focal An integer (default 1) indicating which market is the
#'   "focal" market.
#' @slot parmsStart A length `m + 2` vector whose first `m` elements are
#'   starting values for the price coefficients for each of the `m` markets.
#'   The final 2 elements are starting values for `shape1` and `shape2` of
#'   the [stats::Beta] distribution.
#' @slot shareOutParm A length-2 numeric vector of Beta `shape1`/`shape2`
#'   parameters governing the outside-share default threshold. `NA` (the
#'   default) triggers calibration via moment matching in `calcSlopes`.
#' @slot control.cub A list of [cubature::cubintegrate()] control parameters
#'   used for the multi-market, non-uniform-density outside-share
#'   integration.
#'
#' @section Extends:
#' `LogitDebt`: Class `Logit`, directly. Class `Bertrand`, by class `Logit`,
#'   distance 2. (Legacy defect fixed: the legacy roxygen docs referred to a
#'   misspelled `Bertran` class; corrected to `Bertrand` here.)
#'
#' @section Simplification from the legacy class:
#' The legacy `LogitDebt` representation additionally declared
#' `hFocalIntegral`, `gFocalIntegral`, `tOtherIntegral`, and `rOtherIntegral`
#' function-valued slots. `hFocalIntegral`/`gFocalIntegral` were per-object
#' closures over `shareOutParm` that duplicated the partial-Beta-moment
#' integrals now provided by the shared, oracle-validated
#' `beta_h_focal_integral()`/`beta_g_focal_integral()` helpers in
#' `shared-beta-moments.R` -- the migrated `calcShares`/`calcSlopes` methods
#' call those shared helpers directly (passing `shareOutParm` as explicit
#' arguments) instead of storing per-object closures, so these two slots are
#' no longer needed. `tOtherIntegral`/`rOtherIntegral` were dead code in the
#' legacy package: they were extracted from the object in
#' `calcShares(LogitDebt)` but never invoked (the actual multi-market
#' "other-market" integrals are computed inline via local
#' `integrateDerivOtherFun`/`integrateInsideOtherFun` closures or the
#' `otherFun`/`tFun`/`rFun` closures already local to that method). Both
#' slots are dropped from the migrated class; this is a representational
#' simplification only, verified not to change any computed economics (no
#' call sites existed for either slot in the legacy source).
#'
#' @author Charles Taragin \email{ctaragin+antitrustr@gmail.com}
#' @examples
#' showClass("LogitDebt") # get a detailed description of the class
#'
#' @name LogitDebt-class
#' @aliases LogitDebt-class
#' @rdname LogitDebt-class
#' @include shared-beta-moments.R
#' @export
setClass(
    Class = "LogitDebt",
    contains = "Logit",
    representation = representation(
        marketID = "character",
        productID = "character",
        debtPre = "numeric",
        debtPost = "numeric",
        density = "character",
        focal = "numeric",
        parmsStart = "numeric",
        shareOutParm = "numeric",
        control.cub = "list"
    ),
    prototype = prototype(
        normIndex = 1,
        shareInside = 1,
        focal = 1,
        shareOutParm = c(NA_real_, NA_real_),
        density = "beta",
        control.slopes = list(
            factr = 1e7
        ),
        control.cub = list(
            method = "pcubature",
            nVec = 1024L,
            relTol = 1e-5,
            absTol = 2e-5,
            maxEval = 10^6
        )
    ),
    validity = function(object) {
        nFirms <- length(unique(object@ownerPre))
        nMarkets <- length(unique(object@marketID))

        if (length(object@debtPre) != length(object@shares) ||
            length(object@debtPre) != length(object@debtPost)) {
            stop("'debtPre' and 'debtPost' must equal the number of products")
        }

        if (length(object@mktSize) != nMarkets) {
            stop("'mktSize' must be of length ", nMarkets)
        }

        nMargins <- length(object@margins[!is.na(object@margins)])
        if (nMargins < nMarkets + 2) {
            stop("At least", nMarkets + 2, "elements of 'margins' must not be NA in order to calibrate demand parameters")
        }

        if (length(object@shareOutParm) != 2 ||
            !(all(is.na(object@shareOutParm)) ||
              (all(is.finite(object@shareOutParm)) && all(object@shareOutParm > 0)))) {
            stop("'shareOutParm' must be a length-2 numeric vector with positive values")
        }

        if (!isTRUE(all.equal(
            as.numeric(tapply(object@shares, object@marketID, sum, na.rm = TRUE)),
            rep(1, nMarkets),
            check.attributes = FALSE, tolerance = 1e-3
        ))) {
            stop("sum of 'shares' must equal 1")
        }

        if (any(object@margins < 0 | object@margins > 1, na.rm = TRUE)) {
            stop("'margins should be expressed as a proportion between 0 and 1'")
        }

        if (length(object@parmsStart) != nMarkets + 2) {
            stop("'parmsStart' must be a vector of length ", nMarkets + 2)
        }

        return(TRUE)
    }
)

#' A FinancialFit-compatible wrapper for the debt / limited-liability family
#'
#' `DebtFit` is the concrete `FinancialFit` subclass for the debt model
#' family (see `design/ARCHITECTURE.md`'s "Fit / lifecycle contract"). It
#' does not multiply-inherit from `Logit`/`Bertrand`/`Antitrust`: instead it
#' follows the SAME composition pattern `antitrust` itself uses for
#' `AntitrustFit` (which holds a legacy `Logit`/`Bertrand`/... S4 object in
#' its inherited `model` slot rather than deriving from it). `DebtFit@model`
#' holds the calibrated `LogitDebt` legacy result object; `DebtFit`
#' contributes no additional slots of its own beyond what `StructuralFit`
#' (via `FinancialFit`) already provides (`spec`, `model`, `parameters`,
#' `observed`, `diagnostics`).
#'
#' See [debt_fit()] for the constructor, and the roxygen note on
#' `design-question: LogitDebt multiple inheritance` (in this file's source)
#' for why composition was chosen over multiple inheritance, even though
#' multiple inheritance was verified to be mechanically possible.
#'
#' @name DebtFit-class
#' @aliases DebtFit-class
#' @rdname DebtFit-class
#' @export
setClass(
    Class = "DebtFit",
    contains = "FinancialFit"
)

## -----------------------------------------------------------------------
## Design note (not exported as user-facing docs): LogitDebt multiple
## inheritance was explicitly evaluated per the migration brief before
## choosing composition (DebtFit@model <- <LogitDebt object>) instead.
##
## Verified experimentally against the antitrust@refactor library
## (0.99.38): `setClass("LogitDebt", contains = c("Logit", "DebtFit"))`
## instantiates without error -- there is no slot-name collision between
## Logit's representation and StructuralFit's (spec/model/parameters/
## observed/diagnostics), and FinancialFit itself contributes no
## representation of its own, so mechanically nothing prevents a
## multiply-inherited class.
##
## Composition was chosen anyway because it mirrors the ALREADY-ESTABLISHED
## precedent `antitrust` itself uses for the identical StructuralFit/legacy-
## S4-model boundary: `calibrate()`/`specify()` build a legacy Logit/
## Bertrand/... object with the existing constructors, then wrap it in
## `AntitrustFit` via `model = <legacy object>` (see
## `ModelArchitecture.R`'s `calibrate()`), never via multiple inheritance.
## `design/ARCHITECTURE.md`'s own "Fit / lifecycle contract" diagram also
## draws `DebtFit`/`BankFit` as disjoint CONCRETE subclasses of
## `FinancialFit`, not as siblings mixed into the legacy demand-class
## hierarchy. Given that antitrust's own precedent already answers the
## question the migration brief flagged as possibly needing escalation,
## this is reported as a considered judgment call (see the final report),
## not an unresolved design question -- but it is flagged for architecture-
## lead confirmation since the brief explicitly asked for it to be surfaced.
## -----------------------------------------------------------------------
NULL
