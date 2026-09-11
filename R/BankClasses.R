#' Bank liquidity / stability class definitions
#'
#' S4 classes migrated from the `stability` analysis project (SHA `cc95e2e0`)
#' to model bank Nash-in-rates competition with an endogenous liquidity-risk
#' marginal-cost shifter. Per `design/ARCHITECTURE.md`'s "Fit hierarchy: WRAP,
#' do not multiply-inherit (RULING)": `BankFit contains "FinancialFit"` and
#' holds the bank pricing/state object in `@model`; that inner object
#' `contains "Antitrust"` to reuse `ownerToMatrix()`/`ownerToVec()` instead of
#' redeclaring the ownership-slot contract.
#'
#' @section `BankMarket`:
#' `BankMarket` is the bank-family analogue of `LogitDebt`: it holds the
#' bank-level Nash-in-rates pricing state (ownership, rates, marginal-cost
#' bookkeeping -- all inherited from `Antitrust`) PLUS the row-level
#' bank-market panel and liquidity-friction primitives needed to reconstruct
#' conditional shares and price the liquidity shadow cost `Lambda`
#' (`BankRisk.R`'s `calc_portfolio_liquidity()`).
#'
#' Ownership (`ownerPre`/`ownerPost`/`labels`, inherited from `Antitrust`) is
#' at the BANK level: length `J` (the number of active banks), one label per
#' bank. `pricePre`/`pricePost`/`mcPre`/`mcPost` (also inherited, `numeric`)
#' are length `2J`: the first `J` elements are loan rates `r_L`, the second
#' `J` are deposit rates `r_D`, per bank, in the same bank order as `labels`.
#' `Antitrust`'s own validity method only constrains `labels`/`ownerPre`/
#' `ownerPost` to a common length (verified experimentally against the
#' antitrust@refactor library, `0.99.38`: `pricePre`/`mcPre` length is NOT
#' separately validated), so this `2J`-length convention is compatible with
#' `contains = "Antitrust"` without friction -- see the migration report for
#' the verification.
#'
#' The row-level bank-market panel (`marketID`, `bankRow`, `marketSize_L`/
#' `_D`, `sCondBase_L`/`_D`, `rBase_L`/`_D`, `alpha_L`/`_D`, `outsideMu_L`/
#' `_D`, `phiOut_L`/`_D`, `costRaw_L`/`_D`, `riskFreeRate`) is length `N`
#' (bank-market rows), analogous to `stability`'s `df_year` panel in
#' `simulate_merger_exposure.R`, but WITHOUT that script's Stan-panel-specific
#' `ref_bank_id`/`ref_row_id`/`out_id` bookkeeping (institution-bound
#' plumbing tied to the original G-group Stan panel -- out of migration
#' scope per `design/ARCHITECTURE.md`'s "No DB/FRED/QCEW ingestion"). Instead,
#' each market's conditional-share utility is normalized against that
#' market's FIRST row (the same `normIndex`-relative meanval-inversion
#' convention `LogitDebt`/`logit.debt()` already uses -- see
#' `DebtEquilibrium.R`'s `calcSlopes`), rather than a caller-supplied
#' reference row. This is a representational simplification for a
#' fresh/portable caller (see the migration report's "Z_psi-without-
#' original-panel" discussion); the underlying conditional-logit-with-
#' liquidity-shifted-marginal-cost economics are unchanged.
#'
#' @slot marketID A length N character vector identifying the market of each
#'   bank-market row.
#' @slot bankRow A length N character vector identifying which bank (must
#'   match a `labels` value) each row belongs to.
#' @slot marketSize_L,marketSize_D Length N numeric vectors of loan/deposit
#'   market size for each row.
#' @slot sCondBase_L,sCondBase_D Length N numeric vectors of baseline
#'   (pre-merger observed) conditional loan/deposit shares. Renormalized to
#'   sum to 1 within each market at construction time.
#' @slot rBase_L,rBase_D Length N numeric vectors of baseline (pre-merger
#'   observed) loan/deposit rates.
#' @slot alpha_L,alpha_D Length N numeric vectors of the calibrated
#'   (row-broadcast) price coefficients on loans (expected negative) and
#'   deposits (expected positive), following the Stan sign convention in
#'   `design/STAN-BANKFIT-MAPPING.md`.
#' @slot outsideMu_L,outsideMu_D Length N numeric vectors of the calibrated
#'   Beta participation mean for the outside/non-participation share.
#' @slot phiOut_L,phiOut_D Length N numeric vectors of the calibrated Beta
#'   participation precision.
#' @slot costRaw_L,costRaw_D Length N numeric vectors of the row-level cost
#'   shifter (`W_L \%*\% eta_L`, `W_D \%*\% eta_D` in Stan-primitive terms).
#' @slot riskFreeRate Length N numeric vector of the row-level risk-free
#'   rate.
#' @slot equity,liquidAssets Length J (bank-level) numeric vectors of equity
#'   and the liquid-asset buffer.
#' @slot psi Length J (bank-level) numeric vector of the liquidity-friction
#'   parameter (see `BankBalanceSheet.R`'s `calc_bank_psi()`).
#' @slot omega_L,omega_D Length J (bank-level) numeric vectors of the
#'   calibrated pricing residual (backed out at baseline; held fixed across
#'   counterfactual scenarios, mirroring `simulate_merger_exposure.R`'s
#'   `omega_L`/`omega_D`).
#' @slot liquidityPressure Length J (bank-level) numeric vector of the
#'   calibrated mean liquidity-stress state `mu_W` target for each bank.
#' @slot baseLambda_L,baseLambda_D,baseSigma_W,baseMean_L,baseMean_D Length J
#'   (bank-level) numeric vectors of each bank's OWN (unpooled) baseline
#'   liquidity shadow cost, portfolio SD, and expected loan/deposit exposure,
#'   computed once at calibration time from each bank's own pre-merger
#'   rows/shares (`baseMean_L`/`baseMean_D` are `calc_portfolio_liquidity()`'s
#'   `mean_L`/`mean_D` fields, needed to reproduce
#'   `simulate_merger_exposure.R`'s `liquidity_pressure_ab` exposure-delta
#'   formula on merger). `BankEquilibrium.R`'s `calcMC()` uses these as
#'   the FROZEN liquidity state for any bank whose ownership group does not
#'   change in the scenario being evaluated (and for every bank when
#'   `dynamicLiquidity = FALSE`) -- mirroring `simulate_merger_exposure.R`'s
#'   `pre_liq` object, which is computed once and only overridden for the
#'   ownership groups that actually pool (see `design/ARCHITECTURE.md`:
#'   "Merger changes ownership internalization and pools the combined
#'   entity's liquidity distribution" -- pooling is a property of merging
#'   groups, not a per-iteration share update for untouched banks).
#' @slot reoptimizing Length J (bank-level) logical vector: `TRUE` for banks
#'   that reoptimize rates in the current scenario (always `TRUE` for the
#'   merging parties); `FALSE` for banks whose rates are held fixed (the
#'   `simulate_merger_exposure.R` "exposure-based" approximation, per
#'   `design/ARCHITECTURE.md`'s "the exposure-based solver is the primary
#'   one").
#' @slot loanRateUpper,depositRateUpper Length-1 numeric bounds on solved
#'   rates.
#' @slot psiRule Length-1 character control for how a merged bank's `psi` is
#'   set when liquidity is pooled (`"acquirer"`, `"target"`, `"min"`, or
#'   `"asset_weighted"`, matching `simulate_merger_exposure.R`'s
#'   `CONFIG$psi_rule`).
#' @slot control.equ A list of fixed-point/Broyden solver control parameters
#'   (`damping`, `tol`, `max_iter`).
#'
#' @section Extends:
#' `BankMarket`: Class `Antitrust`, directly.
#'
#' @author Charles Taragin \email{ctaragin+antitrustr@gmail.com}
#' @name BankMarket-class
#' @aliases BankMarket-class
#' @rdname BankMarket-class
#' @export
setClass(
    Class = "BankMarket",
    contains = "Antitrust",
    representation = representation(
        marketID = "character",
        bankRow = "character",
        marketSize_L = "numeric",
        marketSize_D = "numeric",
        sCondBase_L = "numeric",
        sCondBase_D = "numeric",
        rBase_L = "numeric",
        rBase_D = "numeric",
        alpha_L = "numeric",
        alpha_D = "numeric",
        outsideMu_L = "numeric",
        outsideMu_D = "numeric",
        phiOut_L = "numeric",
        phiOut_D = "numeric",
        costRaw_L = "numeric",
        costRaw_D = "numeric",
        riskFreeRate = "numeric",
        equity = "numeric",
        liquidAssets = "numeric",
        psi = "numeric",
        omega_L = "numeric",
        omega_D = "numeric",
        liquidityPressure = "numeric",
        baseLambda_L = "numeric",
        baseLambda_D = "numeric",
        baseSigma_W = "numeric",
        baseMean_L = "numeric",
        baseMean_D = "numeric",
        reoptimizing = "logical",
        loanRateUpper = "numeric",
        depositRateUpper = "numeric",
        psiRule = "character"
    ),
    prototype = prototype(
        output = TRUE,
        loanRateUpper = 0.25,
        depositRateUpper = 0.20,
        psiRule = "asset_weighted",
        control.equ = list(damping = 0.5, tol = 1e-6, max_iter = 250)
    ),
    validity = function(object) {
        nBanks <- length(object@labels)
        nRows <- length(object@marketID)

        rowSlotsN <- c(
            "bankRow", "marketSize_L", "marketSize_D", "sCondBase_L", "sCondBase_D",
            "rBase_L", "rBase_D", "alpha_L", "alpha_D", "outsideMu_L", "outsideMu_D",
            "phiOut_L", "phiOut_D", "costRaw_L", "costRaw_D", "riskFreeRate"
        )
        badRow <- rowSlotsN[vapply(rowSlotsN, function(s) length(slot(object, s)) != nRows, logical(1))]
        if (length(badRow) > 0) {
            stop("The following row-level slots must have length ", nRows, " (nrow(marketID)): ",
                paste(badRow, collapse = ", "))
        }

        bankSlotsN <- c(
            "equity", "liquidAssets", "psi", "omega_L", "omega_D",
            "liquidityPressure", "baseLambda_L", "baseLambda_D", "baseSigma_W",
            "baseMean_L", "baseMean_D", "reoptimizing"
        )
        badBank <- bankSlotsN[vapply(bankSlotsN, function(s) length(slot(object, s)) != nBanks, logical(1))]
        if (length(badBank) > 0) {
            stop("The following bank-level slots must have length ", nBanks, " (length(labels)): ",
                paste(badBank, collapse = ", "))
        }

        if (!all(object@bankRow %in% object@labels)) {
            stop("'bankRow' must only reference bank IDs present in 'labels'")
        }

        if (length(object@pricePre) > 0 && length(object@pricePre) != 2 * nBanks) {
            stop("'pricePre' must have length 2 * length(labels) (loan rates then deposit rates)")
        }
        if (length(object@pricePost) > 0 && length(object@pricePost) != 2 * nBanks) {
            stop("'pricePost' must have length 2 * length(labels) (loan rates then deposit rates)")
        }

        return(TRUE)
    }
)

#' A `FinancialFit`-compatible wrapper for the bank liquidity / stability family
#'
#' `BankFit` is the concrete `FinancialFit` subclass for the bank module
#' family (see `design/ARCHITECTURE.md`'s "Fit / lifecycle contract" and
#' "Fit hierarchy: WRAP, do not multiply-inherit (RULING)"). Mirrors
#' `DebtFit` exactly: it does not multiply-inherit from `Antitrust`, it holds
#' the calibrated `BankMarket` legacy-style result object in its inherited
#' `model` slot.
#'
#' For a multi-draw posterior (see [bank_fit()] and
#' `design/STAN-BANKFIT-MAPPING.md`), `BankFit@model` holds the `BankMarket`
#' built from the FIRST draw (or the point estimate), and `BankFit@diagnostics$draws`
#' holds the full list of per-draw `BankMarket` objects plus the draw matrix,
#' so that [simulate()] can map over all draws.
#'
#' @name BankFit-class
#' @aliases BankFit-class
#' @rdname BankFit-class
#' @export
setClass(
    Class = "BankFit",
    contains = "FinancialFit"
)
