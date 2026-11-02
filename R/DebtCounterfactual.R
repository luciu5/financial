#' Merger simulation with debt (legacy-name constructor)
#'
#' `logit.debt()` calibrates consumer demand using Logit and then simulates
#' the price effect of a merger between two firms under the assumption that
#' all firms in the market are playing a differentiated-products Bertrand
#' pricing game with firm-level debt. Limited liability enters via a
#' default-threshold outside share (see `design/ARCHITECTURE.md`).
#'
#' This is a **parity migration** of `MergerDebt::logit.debt()` (SHA
#' `ea727430`, `R/DebtFunctions.R`): the legacy function name, argument
#' order, and default values are preserved exactly so that existing calling
#' code and the golden-master oracle fixtures continue to apply unchanged.
#' The only behavioral changes are the documented legacy-defect fixes below;
#' none of them alter the economics for any of the golden-master fixture
#' inputs (verified in `tests/testthat/test-debt-parity.R`).
#'
#' Let k denote the number of products produced by all firms across all
#' markets. Let f denote the number of firms producing products across all
#' markets. Let m denote the number of markets.
#'
#' @include DebtEquilibrium.R
#' @param shares A length k vector of product (quantity) shares. Values must
#'   be between 0 and 1.
#' @param prices A length k vector of product prices.
#' @param margins A length k vector of product margins in proportions
#'   (bounded between 0 and 1), some of which may equal `NA`.
#' @param ownerPre A length k vector whose values indicate which firm
#'   produced a product pre-merger.
#' @param ownerPost A length k vector whose values indicate which firm
#'   produced a product post-merger.
#' @param debtPre a length k vector of pre-merger FIRM-level debt payments.
#'   Note that the sum of product-level debt payments should equal firm
#'   level debt.
#' @param debtPost a length k vector of post-merger debt payments. Default
#'   equals `debtPre`.
#' @param marketID A length k vector whose values indicate which market a
#'   product is sold in.
#' @param productID A length k vector whose values indicate a product's
#'   identity.
#' @param mktSize A length m vector equal to market size. If shares sum to
#'   one, this also equals the size of the market.
#'
#'   Legacy defect fixed: the legacy default was
#'   `rep(1, unique(marketID))`, which passes a VECTOR (not a length) as
#'   `times` to `rep()` and errors for any market ID vector where
#'   `unique(marketID)` is not itself a length-1 value (i.e. whenever
#'   `nMarkets > 1`, since `unique()` returns the distinct ID values, not
#'   their count). Fixed to `rep(1, length(unique(marketID)))`.
#' @param density A length 1 vector equal to either `"beta"` (default) or
#'   `"uniform"`. If `"beta"`, the calibration routine assumes firm outside
#'   shares are sampled from a [stats::Beta] distribution and uses margin
#'   information to calibrate `shape1` and `shape2`. If `"uniform"`, then
#'   `shape1 = shape2 = 1`.
#' @param normIndex An integer specifying the product index against which
#'   the mean values of all other products in a market are normalized.
#' @param focal An integer (default 1) indicating which market is the
#'   "focal" market.
#' @param mcDelta A length k vector where each element equals the
#'   proportional change in a downstream firm's product-level marginal costs
#'   due to the merger. Default is 0, which assumes that the merger does not
#'   affect any products' marginal cost (debt does NOT modify marginal cost;
#'   see `design/ARCHITECTURE.md`).
#' @param priceOutside A length 1 vector indicating the price of the outside
#'   good. Default is 0.
#' @param priceStart A length k vector of starting values used to solve for
#'   downstream equilibrium prices. Default is the `prices` vector.
#' @param parmsStart A length `m + 2` vector whose first `m` elements are
#'   starting values for the price coefficients for each of the `m` markets.
#'   The final 2 elements are starting values for `shape1` and `shape2` of
#'   the [stats::Beta] distribution.
#' @param shareOutParm A length-2 numeric vector of Beta `shape1`/`shape2`
#'   parameters. If supplied, `density` calibration is skipped and this
#'   value is used directly (this is how `density = "uniform"` is
#'   implemented: `shareOutParm <- c(1, 1)`).
#' @param isMax If `TRUE`, runs a numerical check (via `numDeriv::genD()`,
#'   now explicitly namespace-qualified -- see Details) of whether the
#'   computed equilibrium price vector is a local profit maximum. Default is
#'   `FALSE`.
#' @param control.slopes A list of [stats::optim()] control parameters
#'   passed to the calibration routine optimizer (the `calcSlopes` method).
#' @param control.equ A list of [BB::BBsolve()] control parameters passed to
#'   the non-linear equation solver (the `calcPrices` method).
#' @param control.cub A list of [cubature::cubintegrate()] control
#'   parameters used for the multi-market, non-uniform-density outside-share
#'   integration.
#' @param labels A k-length vector of labels. Default is
#'   `paste(ownerPre, productID, marketID, sep = ":")`.
#' @param ... Additional options to feed to the [BB::BBsolve()] optimizer
#'   used to solve for equilibrium prices.
#'
#' @details Using product prices, quantity shares, and all of the product
#' margins from at least one firm, `logit.debt` recovers the price
#' coefficient and product mean valuations in a Logit demand model, subject
#' to a firm-level debt/limited-liability default threshold. `logit.debt`
#' then uses these calibrated parameters to simulate a merger between two
#' firms.
#'
#' Legacy defects fixed during migration (see `design/ARCHITECTURE.md`'s
#' "Known legacy defects to fix during migration"):
#' \itemize{
#'   \item `numDeriv::genD` is now explicitly namespace-qualified in
#'     `calcPrices(LogitDebt, isMax = TRUE)` (previously an unqualified
#'     `genD()` call that only worked if `numDeriv` happened to be
#'     attached).
#'   \item The `@examples` block below uses the corrected `mktSize <-
#'     c(1, 1)` for the 2-market example (the legacy doc example set
#'     `mktSize <- 1`, which fails `LogitDebt`'s own validity check that
#'     `length(mktSize) == nMarkets`).
#'   \item The `mktSize` default itself is fixed from
#'     `rep(1, unique(marketID))` to `rep(1, length(unique(marketID)))`
#'     (see the `mktSize` parameter documentation above).
#'   \item The `density` prototype/argument default mismatch is resolved:
#'     `LogitDebt`'s class prototype now defaults to `"beta"`, matching this
#'     constructor's default, instead of the legacy prototype's `"uniform"`.
#'   \item The `Bertran` typo in the legacy roxygen `@section Extends:` docs
#'     is corrected to `Bertrand` (see `LogitDebt-class`).
#' }
#'
#' None of these fixes changes the calibrated/simulated economics for any
#' existing caller that supplies `mktSize` and `density` explicitly (as the
#' golden-master oracle fixtures all do).
#'
#' @return `logit.debt` returns an instance of class
#'   \code{\linkS4class{LogitDebt}}.
#' @author Charles Taragin \email{ctaragin+antitrustr@gmail.com}
#' @examples
#' ## 3 single-product firms, each selling in 2 markets.
#' ## Firms 1 and 2 merge
#' nprods <- 3
#' nmarkets <- 2
#' marketId <- rep(1:nmarkets, nprods)
#' shares <- c(0.156, 0.418, 0.616, 0.424, 0.229, 0.158)
#' prices <- c(97.303, 119.083, 94.103, 147.905, 93.113, 100.935)
#' margins <- c(17.163, 84.026, 26.451, 103.05, 17.316, 64.105) / prices
#' ownerPre <- ownerPost <- as.character(rep(c(1, 2, 3), each = nmarkets))
#' ownerPost[ownerPost == "2"] <- "1"
#' debt <- c(22.69, NA, 51.002, NA, 2.922, NA)
#' mktSize <- c(1, 1) # legacy doc example incorrectly set this to scalar `1`
#'
#' simres_debt <- logit.debt(prices, shares, margins,
#'     ownerPre, ownerPost,
#'     debtPre = debt, marketID = marketId,
#'     mktSize = mktSize
#' )
#'
#' print(simres_debt@pricePre)
#' print(simres_debt@pricePost)
#' @export
logit.debt <- function(prices, shares, margins,
    ownerPre, ownerPost,
    debtPre = rep(0, length(prices)),
    debtPost = debtPre,
    marketID = as.character(rep(1, length(prices))),
    productID,
    mktSize = rep(1, length(unique(marketID))),
    density = c("beta", "uniform"),
    normIndex = 1,
    focal = 1,
    mcDelta = rep(0, length(prices)),
    priceOutside = 0,
    priceStart = prices,
    parmsStart,
    shareOutParm,
    isMax = FALSE,
    control.slopes,
    control.equ,
    control.cub,
    labels,
    ...) {
    density <- match.arg(density)
    if (missing(shareOutParm)) shareOutParm <- rep(NA_real_, 2)
    if (density == "uniform") shareOutParm <- rep(1, 2)

    if (missing(productID)) {
        nFirmProds <- tapply(prices, ownerPre, length)
        productID <- unlist(sapply(nFirmProds, function(x) 1:x), use.names = FALSE)
        productID <- paste0(ownerPre, productID)
    }

    mktData <- data.frame(
        marketID, productID, prices, shares, margins, ownerPre, ownerPost,
        debtPre, debtPost, mcDelta
    )

    if (missing(parmsStart)) {
        ## set starting value of alpha to average of single-product equilibrium margin
        parmsStart <- with(mktData, tapply(-1 / (margins * prices * (1 - shares)), marketID, mean, na.rm = TRUE))
        ## set starting values of shareOutParm to Beta(4,4)
        parmsStart <- c(as.numeric(parmsStart), 4, 4)
    }

    mktData <- mktData[with(mktData, order(ownerPre, productID, marketID)), ]

    if (missing(labels)) labels <- with(mktData, paste(ownerPre, productID, marketID, sep = ":"))
    nMarkets <- length(unique(marketID))

    ## Create LogitDebt container to store relevant data
    result <- new("LogitDebt",
        prices = mktData$prices, shares = mktData$shares,
        margins = mktData$margins,
        debtPre = mktData$debtPre,
        debtPost = mktData$debtPost,
        mktSize = mktSize,
        normIndex = normIndex,
        focal = focal,
        density = density,
        shareOutParm = shareOutParm,
        ownerPre = as.character(mktData$ownerPre),
        ownerPost = as.character(mktData$ownerPost),
        mcDelta = mktData$mcDelta,
        subset = rep(TRUE, length(mktData$prices)),
        marketID = as.character(mktData$marketID),
        productID = as.character(mktData$productID),
        priceStart = priceStart,
        parmsStart = parmsStart,
        labels = labels
    )

    if (!missing(control.slopes)) {
        result@control.slopes <- control.slopes
    }
    if (!missing(control.equ)) {
        result@control.equ <- control.equ
    }
    if (!missing(control.cub)) {
        result@control.cub <- control.cub
    }

    ## Calculate Demand Slope Coefficients
    result <- calcSlopes(result)

    ## Calculate marginal cost
    result@mcPre <- calcMC(result, TRUE)
    result@mcPost <- calcMC(result, FALSE)

    ## Solve Non-Linear System for Price Changes
    result@pricePre <- calcPrices(result, preMerger = TRUE, isMax = isMax, ...)
    result@pricePost <- calcPrices(result, preMerger = FALSE, isMax = isMax, ...)

    return(result)
}

#' Construct a `FinancialFit`-compatible debt fit
#'
#' `debt_fit()` is `financial`'s own constructor (per
#' `design/ARCHITECTURE.md`: "`financial` ships its own constructors
#' `debt_fit()`/`bank_fit()`") for the `DebtFit` `FinancialFit` subclass.
#' Rather than deriving `LogitDebt` from both `Logit` and `FinancialFit`
#' (verified mechanically possible but not the chosen design -- see the
#' design note in `DebtClasses.R` and this migration's final report), it
#' composes: `debt_fit()` calls [logit.debt()] to build the calibrated
#' legacy `LogitDebt` object exactly as before, then wraps it in a `DebtFit`
#' whose `model` slot holds that object. This mirrors the same
#' `StructuralFit@model <- <legacy S4 object>` composition pattern
#' `antitrust` itself uses for `AntitrustFit` (see `calibrate()` in
#' `antitrust`'s `ModelArchitecture.R`).
#'
#' @param ... Arguments forwarded to [logit.debt()].
#' @return A `DebtFit` object whose `model` slot holds the calibrated
#'   `LogitDebt` result, `parameters` holds the calibrated `alpha`/`meanval`
#'   slopes and `shareOutParm`, and `observed` holds the identifying
#'   pre-merger inputs.
#' @export
debt_fit <- function(...) {
    model <- logit.debt(...)

    new("DebtFit",
        spec = list(family = "debt", demand = "logit", conduct = "bertrand"),
        model = model,
        parameters = list(
            alpha = model@slopes$alpha,
            meanval = model@slopes$meanval,
            shareOutParm = model@shareOutParm
        ),
        observed = list(
            prices = model@prices,
            shares = model@shares,
            margins = model@margins,
            ownerPre = model@ownerPre,
            debtPre = model@debtPre,
            marketID = model@marketID,
            labels = model@labels
        ),
        diagnostics = list(
            status = "completed",
            model_class = "LogitDebt",
            density = model@density
        )
    )
}

#' `simulate()`/`simulate_steps()`/`respecify()` for `DebtFit`
#'
#' Lifecycle methods for the `FinancialFit` generics on `DebtFit`, satisfying
#' `design/ARCHITECTURE.md`'s "Lifecycle methods implemented only where
#' economically meaningful; unsupported ops error explicitly."
#'
#' `simulate(DebtFit, ownerPost, ...)` re-solves the debt model at a new
#' post-merger ownership structure (and, optionally, new post-merger debt via
#' `debtPost`), returning an updated `DebtFit`. This is the exposure-only
#' counterfactual path described in `design/ARCHITECTURE.md`'s
#' "Counterfactual uncertainty response": ownership/leverage change; the
#' Beta outside-share law (`shareOutParm`/`density`) is held fixed unless the
#' caller explicitly opts in via `shareOutParm`.
#'
#' `simulate_steps()` and `respecify()` are NOT implemented for `DebtFit` in
#' this migration wave: sequential multi-step `CounterfactualPath` resume
#' and cross-family respecification are real features called for by
#' `design/ARCHITECTURE.md` but require the primitive-shift `uncertainty`
#' step type and cumulative-uncertainty bookkeeping described there, which
#' is explicitly out of scope for a parity migration of the existing
#' single-step legacy economics. They fail loudly per the `FinancialFit`
#' default-reject contract (inherited unchanged; see below) rather than
#' silently no-oping.
#'
#' @param object A `DebtFit`.
#' @param ownerPost A post-merger ownership vector, matrix, or `NULL` to
#'   reuse `object`'s currently fitted post-merger ownership.
#' @param debtPost Optional post-merger firm-level debt vector (length
#'   matching the fitted products); default reuses the currently fitted
#'   `debtPost`.
#' @param ... Additional arguments forwarded to [calcPrices()] (e.g.
#'   `isMax`).
#' @return An updated `DebtFit`.
#' @name DebtFit-lifecycle
#' @rdname DebtFit-lifecycle
NULL

#' Debt-module counterfactual uncertainty deltas
#'
#' Computes, per firm, how a merger changes the uncertainty the firm faces --
#' the debt-module counterpart to the bank module's `uncertainty_delta` output
#' (`design/ARCHITECTURE.md`, "Counterfactual uncertainty response",
#' exposure-only path). The Beta outside-share law (`shareOutParm`) is held
#' fixed; the merger moves each firm's default threshold via ownership pooling
#' and `debtPost`.
#'
#' The threshold `s0Focal` is the outside-share level at or above which the
#' firm's profit falls short of its debt (from `s0Focal = 1 - debt/profit` in
#' the single-market case, generalized across markets in
#' `calcShares(outSideOnly = TRUE)`). Under the calibrated Beta with shape
#' parameters `shareOutParm`, the firm defaults when the realized outside share
#' exceeds the threshold, so:
#'   - default probability     = `beta_tail_prob(s0Focal)`  (P(u > s0Focal))
#'   - survival-weighted profit = per-firm conditional profit weighted by the
#'     lower partial Beta mass `beta_lower_prob(s0Focal)` (the survival region).
#' Deltas are post-merger minus pre-merger. A negative `delta_default_prob`
#' is the debt/insurance effect (merger raises pooled profit -> raises the
#' survival threshold -> lowers default risk).
#'
#' @keywords internal
#' @noRd
.debt_uncertainty_delta <- function(model) {
    sp <- model@shareOutParm
    ## Uncalibrated / degenerate Beta (e.g. uniform with NA/1,1): still report,
    ## using whatever shareOutParm the fit carries.
    s1 <- sp[1]
    s2 <- sp[2]

    thresh <- function(pre) {
        ## SharesOutInt: nMarkets x nFirms outside-share thresholds. Collapse to
        ## one threshold per firm (focal-market row) for the summary delta.
        so <- calcShares(model, preMerger = pre, outSideOnly = TRUE)
        so <- as.matrix(so)
        foc <- model@focal
        stats::setNames(as.numeric(so[foc, ]), colnames(so))
    }

    s0_pre <- thresh(TRUE)
    s0_post <- thresh(FALSE)

    ## Align on the union of firm labels present pre/post (post may re-label
    ## merging firms to a common owner id).
    default_prob <- function(s0) beta_tail_prob(s0, s1, s2)
    survival_mass <- function(s0) beta_lower_prob(s0, s1, s2)

    firms <- union(names(s0_pre), names(s0_post))
    pick <- function(v, f) if (f %in% names(v)) v[[f]] else NA_real_

    dp_pre <- vapply(firms, function(f) default_prob(pick(s0_pre, f)), numeric(1))
    dp_post <- vapply(firms, function(f) default_prob(pick(s0_post, f)), numeric(1))
    sv_pre <- vapply(firms, function(f) survival_mass(pick(s0_pre, f)), numeric(1))
    sv_post <- vapply(firms, function(f) survival_mass(pick(s0_post, f)), numeric(1))

    list(
        firm = firms,
        threshold_pre = stats::setNames(vapply(firms, function(f) pick(s0_pre, f), numeric(1)), firms),
        threshold_post = stats::setNames(vapply(firms, function(f) pick(s0_post, f), numeric(1)), firms),
        default_prob_pre = stats::setNames(dp_pre, firms),
        default_prob_post = stats::setNames(dp_post, firms),
        delta_default_prob = stats::setNames(dp_post - dp_pre, firms),
        delta_survival_mass = stats::setNames(sv_post - sv_pre, firms)
    )
}

#' @rdname DebtFit-lifecycle
#' @export
setMethod("simulate", "DebtFit", function(object, ownerPost = NULL, debtPost = NULL, ...) {
    model <- object@model

    if (!is.null(ownerPost)) model@ownerPost <- as.character(ownerPost)
    if (!is.null(debtPost)) model@debtPost <- as.numeric(debtPost)

    model@mcPost <- calcMC(model, preMerger = FALSE)
    model@pricePost <- calcPrices(model, preMerger = FALSE, ...)

    object@model <- model
    object@diagnostics$route <- "simulate"
    ## First-class uncertainty-delta output (ARCHITECTURE "Counterfactual
    ## uncertainty response"): how the merger changes each firm's default risk.
    object@diagnostics$uncertainty_delta <- tryCatch(
        .debt_uncertainty_delta(model),
        error = function(e) list(error = conditionMessage(e))
    )
    object
})

## simulate_steps() / respecify(): intentionally NOT overridden for DebtFit.
## They inherit FinancialFit's default-reject methods (FinancialArchitecture.R),
## which stop() with an explicit, family-named message rather than silently
## falling through to unrelated economics. See the roxygen note above.
