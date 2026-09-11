#' Nash-in-rates equilibrium methods for the `BankMarket` class
#'
#' `calcShares`, `calcMC`, `calcMargins`, `calcPrices` methods for
#' `BankMarket`, migrated (parity migration of the economics, generalized
#' representation -- see below) from the exposure-based footprint-wide
#' merger solver in `stability/code/simulate_merger_exposure.R` (SHA
#' `cc95e2e0`): the damped fixed-point solver with an `nleqslv` Broyden
#' fallback (`solve_equilibrium_exposure()`/`pricing_map_exposure()`), the
#' bank-level Nash-in-rates first-order conditions
#' (`build_deriv_matrices()`), and liquidity pooling on merger
#' (`pricing_map_exposure()`'s `dynamic_liquidity` branch calling
#' `calc_portfolio_liquidity()` from `BankRisk.R`).
#'
#' @section Key structural fact preserved from the legacy script:
#' `simulate_merger_exposure.R` never "solves" a pre-merger equilibrium --
#' pre-merger rates, shares, markups, cost shifters, and liquidity shadow
#' costs are all directly OBSERVED/measured from the panel (`STEP 3:
#' Calibrate Base Utility & Pre-Merger Baseline`), and used only to back out
#' the pricing residual `omega` and the liquidity-pressure target
#' (`calibrate_liquidity_pressure_from_wedges()`, `BankBalanceSheet.R`). The
#' fixed-point/Broyden solver (`solve_equilibrium_exposure()`) is invoked
#' ONLY for post-merger counterfactual scenarios, always under
#' `ownerPost_mat` (both the "Competition-only" `dynamic_liquidity = FALSE`
#' and "Full Merger" `dynamic_liquidity = TRUE` scenarios use
#' `ownerPost_mat`; they differ only in whether liquidity is pooled). This
#' migration preserves that structure exactly: `calcShares`/`calcMargins`/
#' `calcMC(preMerger = TRUE)` read straight from `object`'s baseline slots
#' (`sCondBase_L`/`_D`, `outsideMu_L`/`_D`, `baseLambda_L`/`_D`, ...) with NO
#' price-implied recomputation; `calcPrices(preMerger = TRUE)` simply
#' returns `object@pricePre` (already-observed data, not solved). Only the
#' `preMerger = FALSE` methods run the price-implied share/margin/liquidity
#' recomputation and the fixed-point/Broyden equilibrium search.
#'
#' @section Generalization from the legacy 2-bank exposure script:
#' `simulate_merger_exposure.R` is written for exactly ONE pairwise merger
#' (`bank_a_id`/`bank_b_id`) and special-cases their 2x2 FOC block. The
#' economics generalize losslessly to an arbitrary ownership structure: the
#' FOC system `ownerMat * t(deriv)` IS genuinely block-diagonal by ownership
#' GROUP (common ownership is an equivalence relation), so this migration
#' solves each ownership-group block directly (`.bank_solve_markup_block()`,
#' partitioned by owner label -- NOT by the FOC matrix's incidental zero
#' pattern, which undercounts chain-connected 3+-bank groups; see the
#' correctness note on that function) instead of hard-coding a 2x2 case. This
#' supports mergers among more than two banks and multiple simultaneous
#' merging pairs, a strict generalization that reduces to the legacy 2-bank
#' behavior exactly when there is exactly one 2-bank merging pair.
#'
#' @section Generalization: no exposure-threshold bank/market selection:
#' `simulate_merger_exposure.R`'s `STEP 2` iteratively searches for an
#' exposure threshold that keeps the "reoptimizing" competitor-bank/market
#' set below a size cap, for tractability on a specific large real-world
#' panel. That search is data-selection PLUMBING, not part of the priced
#' economics (`design/ARCHITECTURE.md` scopes this migration to "solver +
#' primitives only"). `BankMarket@reoptimizing` is instead supplied directly
#' by the caller (default: all banks reoptimize) -- the FOC/fixed-point/
#' liquidity-pooling economics that operate on that set are migrated
#' unchanged.
#'
#' @section Generalization: no Stan G-group reference-row panel:
#' The legacy script inverts each row's mean utility relative to a
#' `ref_bank_id`/`ref_row_id` supplied by the original Stan panel (see
#' `design/STAN-BANKFIT-MAPPING.md`'s Z_psi UNKNOWN). A fresh `BankFit`
#' caller has no such panel. This migration uses the SAME closed-form
#' logit-inversion trick `LogitDebt`/`logit.debt()` already use for their
#' `normIndex` product (`DebtEquilibrium.R`'s `calcSlopes`): within each
#' market, the FIRST row (in the row order supplied to the constructor) is
#' the reference row, and the counterfactual mean-utility offset is
#' recovered relative to it (`.bank_base_utility()`). This is the same
#' mathematical operation at variable-market-size granularity, not a new
#' economic assumption.
#'
#' @name BankMarket-methods
#' @param object A `BankMarket`.
#' @param preMerger When `TRUE`, return the OBSERVED baseline (data, not
#'   solved). When `FALSE`, return the counterfactual/post-merger regime
#'   (`ownerPost`); requires `object@pricePost` to have been solved (via
#'   [calcPrices()]) for `calcShares()`/`calcMargins()`/`calcMC()`.
#' @param dynamicLiquidity When `TRUE` (only meaningful for `preMerger =
#'   FALSE`), banks whose ownership CHANGED between `ownerPre` and
#'   `ownerPost` have their liquidity distributions POOLED (per
#'   `design/ARCHITECTURE.md`: "Merger changes ownership internalization and
#'   pools the combined entity's liquidity distribution") before pricing
#'   `Lambda`. When `FALSE`, every bank keeps its own frozen baseline
#'   liquidity shadow cost even post-merger -- this is the
#'   "Competition-only" counterfactual scenario in
#'   `simulate_merger_exposure.R` (ownership/pricing changes but liquidity
#'   risk is NOT pooled), used as a decomposition device in
#'   `BankCounterfactual.R`.
#' @param ... Additional arguments (currently unused; retained for generic
#'   compatibility).
#' @include BankBalanceSheet.R
NULL

## ---------------------------------------------------------------------
## Internal row/market bookkeeping.
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.bank_ref_row <- function(object) {
  ave(seq_along(object@marketID), object@marketID, FUN = function(i) i[1])
}

#' @keywords internal
#' @noRd
.bank_base_utility <- function(object) {
  ref <- .bank_ref_row(object)
  list(
    L = log(object@sCondBase_L) - log(object@sCondBase_L[ref]) -
      object@alpha_L * (object@rBase_L - object@rBase_L[ref]),
    D = log(object@sCondBase_D) - log(object@sCondBase_D[ref]) -
      object@alpha_D * (object@rBase_D - object@rBase_D[ref]),
    ref = ref
  )
}

#' Row-level conditional & outside shares at a given bank-level rate vector
#'
#' Generalization of `simulate_merger_exposure.R`'s `pricing_map_exposure()`
#' steps 2-3 (`u_diff_L`/`s_cond_L_next`/`update_outside_moments()`) to an
#' arbitrary market layout (see the `BankMarket-methods` roxygen notes on
#' the reference-row generalization). Used ONLY for the `preMerger = FALSE`
#' (price-implied) regime; the `preMerger = TRUE` baseline reads
#' `object@sCondBase_L`/`_D` directly (see [calcShares()]).
#'
#' @keywords internal
#' @noRd
.bank_calc_shares_row <- function(object, r_L_bank, r_D_bank) {
  rowBank <- match(object@bankRow, object@labels)
  r_L_row <- r_L_bank[rowBank]
  r_D_row <- r_D_bank[rowBank]

  base <- .bank_base_utility(object)
  ref <- base$ref

  u_diff_L <- base$L + object@alpha_L * (r_L_row - r_L_row[ref])
  u_diff_D <- base$D + object@alpha_D * (r_D_row - r_D_row[ref])

  exp_u_L <- exp(u_diff_L)
  exp_u_D <- exp(u_diff_D)
  sum_L <- stats::ave(exp_u_L, object@marketID, FUN = sum)
  sum_D <- stats::ave(exp_u_D, object@marketID, FUN = sum)
  s_cond_L <- exp_u_L / sum_L
  s_cond_D <- exp_u_D / sum_D

  ## Dynamic outside-share update (rho_out defaults to 1, matching
  ## simulate_merger_exposure.R's default when the Stan data omits it).
  rate_eff_L <- object@alpha_L * (r_L_row - object@rBase_L)
  rate_eff_D <- object@alpha_D * (r_D_row - object@rBase_D)
  S_L <- pmax(stats::ave(object@sCondBase_L * exp(rate_eff_L), object@marketID, FUN = sum), 1e-12)
  S_D <- pmax(stats::ave(object@sCondBase_D * exp(rate_eff_D), object@marketID, FUN = sum), 1e-12)
  delta_iv_L <- pmax(pmin(log(S_L), 20), -20)
  delta_iv_D <- pmax(pmin(log(S_D), 20), -20)

  q1_L0 <- beta_inside_mean(object@outsideMu_L)
  q1_D0 <- beta_inside_mean(object@outsideMu_D)
  D0_L <- q1_L0 / (1 - q1_L0)
  D0_D <- q1_D0 / (1 - q1_D0)
  D_L <- D0_L * exp(delta_iv_L)
  D_D <- D0_D * exp(delta_iv_D)
  I_L <- D_L / (1 + D_L)
  I_D <- D_D / (1 + D_D)
  mu_L <- pmin(pmax(1 - I_L, 1e-9), 1 - 1e-9)
  mu_D <- pmin(pmax(1 - I_D, 1e-9), 1 - 1e-9)

  list(
    s_cond_L = s_cond_L, s_cond_D = s_cond_D,
    outside_mu_L = mu_L, outside_mu_D = mu_D,
    q1_L = beta_inside_mean(mu_L), q1_D = beta_inside_mean(mu_D),
    q2_L = beta_inside_second(mu_L, object@phiOut_L),
    q2_D = beta_inside_second(mu_D, object@phiOut_D)
  )
}

#' @keywords internal
#' @noRd
.bank_shares_baseline <- function(object) {
  list(
    s_cond_L = object@sCondBase_L, s_cond_D = object@sCondBase_D,
    outside_mu_L = object@outsideMu_L, outside_mu_D = object@outsideMu_D,
    q1_L = beta_inside_mean(object@outsideMu_L), q1_D = beta_inside_mean(object@outsideMu_D),
    q2_L = beta_inside_second(object@outsideMu_L, object@phiOut_L),
    q2_D = beta_inside_second(object@outsideMu_D, object@phiOut_D)
  )
}

## ---------------------------------------------------------------------
## calcShares(): public entry point.
## ---------------------------------------------------------------------

#' @rdname BankMarket-methods
#' @export
setMethod(
  f = "calcShares",
  signature = "BankMarket",
  definition = function(object, preMerger = TRUE, ...) {
    if (preMerger) {
      return(.bank_shares_baseline(object))
    }
    nBanks <- length(object@labels)
    if (length(object@pricePost) != 2 * nBanks) {
      stop("calcShares(BankMarket, preMerger=FALSE) requires 'pricePost' to be ",
        "set (length 2 * nBanks); run calcPrices(preMerger=FALSE) first.")
    }
    r_L_bank <- object@pricePost[seq_len(nBanks)]
    r_D_bank <- object@pricePost[nBanks + seq_len(nBanks)]
    .bank_calc_shares_row(object, r_L_bank, r_D_bank)
  }
)

## ---------------------------------------------------------------------
## Bank-level expected-quantity weights (calc_footprint_weights equivalent).
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.bank_quantity_weights <- function(object, shares) {
  labels <- object@labels
  rowBank <- match(object@bankRow, labels)
  q_L <- object@marketSize_L * shares$s_cond_L * shares$q1_L
  q_D <- object@marketSize_D * shares$s_cond_D * shares$q1_D
  Q_L <- as.numeric(tapply(q_L, factor(rowBank, levels = seq_along(labels)), sum))
  Q_D <- as.numeric(tapply(q_D, factor(rowBank, levels = seq_along(labels)), sum))
  Q_L[is.na(Q_L)] <- 0
  Q_D[is.na(Q_D)] <- 0

  pi_L <- q_L / pmax(Q_L[rowBank], 1e-12)
  pi_D <- q_D / pmax(Q_D[rowBank], 1e-12)

  list(Q_L = Q_L, Q_D = Q_D, pi_L = pi_L, pi_D = pi_D, rowBank = rowBank)
}

## ---------------------------------------------------------------------
## calcMargins(): bank-level Nash-in-rates FOC markups/markdowns.
## Generalizes build_deriv_matrices() + the 2x2/scalar solves in
## pricing_map_exposure() (see the block-solve generalization note above).
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.bank_deriv_matrices <- function(object, shares, K, bankToIdx) {
  deriv_L <- matrix(0, K, K)
  deriv_D <- matrix(0, K, K)
  rowBank <- match(object@bankRow, object@labels)
  idx <- bankToIdx[rowBank]

  for (m in unique(object@marketID)) {
    rows <- which(object@marketID == m)
    rows <- rows[idx[rows] > 0]
    if (length(rows) == 0) next

    own_L <- object@alpha_L[rows] * object@marketSize_L[rows] *
      (shares$s_cond_L[rows] * shares$q1_L[rows] - shares$s_cond_L[rows]^2 * shares$q2_L[rows])
    own_D <- object@alpha_D[rows] * object@marketSize_D[rows] *
      (shares$s_cond_D[rows] * shares$q1_D[rows] - shares$s_cond_D[rows]^2 * shares$q2_D[rows])

    for (a in seq_along(rows)) {
      i <- idx[rows[a]]
      deriv_L[i, i] <- deriv_L[i, i] + own_L[a]
      deriv_D[i, i] <- deriv_D[i, i] + own_D[a]
      for (b in seq_along(rows)) {
        if (a == b) next
        j <- idx[rows[b]]
        cross_L <- -object@alpha_L[rows[a]] * object@marketSize_L[rows[a]] *
          shares$s_cond_L[rows[a]] * shares$s_cond_L[rows[b]] * shares$q2_L[rows[a]]
        cross_D <- -object@alpha_D[rows[a]] * object@marketSize_D[rows[a]] *
          shares$s_cond_D[rows[a]] * shares$s_cond_D[rows[b]] * shares$q2_D[rows[a]]
        deriv_L[i, j] <- deriv_L[i, j] + cross_L
        deriv_D[i, j] <- deriv_D[i, j] + cross_D
      }
    }
  }
  list(L = deriv_L, D = deriv_D)
}

#' Solve a (possibly block-diagonal) Nash-in-rates FOC system for markups
#'
#' Generalizes the legacy script's per-bank scalar division (unmerged banks)
#' and 2x2 `solve()` (the two merging banks) to an arbitrary ownership-group
#' block structure -- see the `BankMarket-methods` roxygen note on this
#' generalization. `sign = -1` reproduces the legacy loan-side convention
#' (`markup_L <- -Q_L/deriv_L`); `sign = 1` reproduces the legacy deposit-side
#' convention (`markdown_D <- Q_D/deriv_D`).
#'
#' @section Blocks are OWNERSHIP groups, not the FOC matrix's zero pattern:
#' A red-team review (2026-09) found that partitioning by the FOC matrix's
#' incidental zero pattern (one-hop neighbors in `foc[i,] != 0`) is WRONG for
#' chain-connected ownership groups of 3+ banks: if bank A and bank C are
#' commonly owned with B but A and C have no direct cross-derivative (e.g. no
#' shared market), a one-hop scan splits {A,B,C} into overlapping 2-bank
#' blocks {A,B} and {B,C} that are solved and OVERWRITTEN independently,
#' violating A's and B's own FOC by a material margin (verified: residual up
#' to ~0.15 on a synthetic 3-bank chain, vs machine-epsilon for the true joint
#' solve; see `tests/testthat/test-bank-blocksolve.R`). Common ownership is an
#' EQUIVALENCE RELATION (reflexive/symmetric/transitive on "same owner"), so
#' partitioning by `blockId` (one id per distinct owner among the reoptimizing
#' banks) is the economically correct block structure -- it is exact by
#' construction, not an approximation, and the FOC system restricted to that
#' partition is genuinely block-diagonal regardless of which cross-derivatives
#' happen to be nonzero within a group.
#'
#' @param foc A K x K ownership-weighted derivative matrix.
#' @param Q A length-K quantity-weight vector.
#' @param sign +/-1, see above.
#' @param blockId A length-K vector (any type with meaningful `==`) giving the
#'   ownership-group id of each of the K rows/columns; rows with the same
#'   `blockId` are solved as one joint block. Required -- do not infer blocks
#'   from `foc`'s zero pattern.
#' @keywords internal
#' @noRd
.bank_solve_markup_block <- function(foc, Q, sign, blockId) {
  K <- nrow(foc)
  out <- numeric(K)
  for (id in unique(blockId)) {
    block <- which(blockId == id)
    if (length(block) == 1L) {
      out[block] <- if (foc[block, block] != 0) sign * Q[block] / foc[block, block] else 0
      next
    }
    sol <- try(as.numeric(solve(foc[block, block, drop = FALSE], Q[block])), silent = TRUE)
    if (inherits(sol, "try-error")) {
      out[block] <- vapply(block, function(b) {
        if (foc[b, b] != 0) sign * Q[b] / foc[b, b] else 0
      }, numeric(1))
    } else {
      out[block] <- sign * sol
    }
  }
  out
}

#' @rdname BankMarket-methods
#' @export
setMethod(
  f = "calcMargins",
  signature = "BankMarket",
  definition = function(object, preMerger = TRUE, ...) {
    shares <- calcShares(object, preMerger = preMerger)
    weights <- .bank_quantity_weights(object, shares)

    reopt <- which(object@reoptimizing)
    K <- length(reopt)
    bankToIdx <- integer(length(object@labels))
    bankToIdx[reopt] <- seq_along(reopt)

    ownerFull <- ownerToMatrix(object, preMerger = preMerger)
    ownerBlock <- ownerFull[reopt, reopt, drop = FALSE]

    ## Block id = the reoptimizing bank's owner label (common ownership is an
    ## equivalence relation -> the TRUE block partition; see the correctness
    ## note on .bank_solve_markup_block()). Do NOT infer blocks from focL/focD's
    ## zero pattern -- that undercounts chain-connected 3+-bank groups.
    ownerLabels <- if (preMerger) object@ownerPre else object@ownerPost
    blockId <- ownerLabels[reopt]

    derivs <- .bank_deriv_matrices(object, shares, K, bankToIdx)
    focL <- ownerBlock * t(derivs$L)
    focD <- ownerBlock * t(derivs$D)

    markup_L <- numeric(length(object@labels))
    markdown_D <- numeric(length(object@labels))
    markup_L[reopt] <- .bank_solve_markup_block(focL, weights$Q_L[reopt], sign = -1, blockId = blockId)
    markdown_D[reopt] <- .bank_solve_markup_block(focD, weights$Q_D[reopt], sign = 1, blockId = blockId)

    list(
      markup_L = markup_L, markdown_D = markdown_D,
      Q_L = weights$Q_L, Q_D = weights$Q_D,
      shares = shares, weights = weights
    )
  }
)

#' @rdname BankMarket-methods
#' @export
setMethod(
  f = "calcSlopes",
  signature = "BankMarket",
  definition = function(object, ...) {
    ## The bank module ingests already-calibrated Stan-posterior primitives
    ## (via bank_fit(parameters=)); it does not run an antitrust-style
    ## calcSlopes demand-calibration. Fail explicitly (per ARCHITECTURE's
    ## "unsupported operations should fail explicitly") rather than relying on
    ## R's cryptic "unable to find an inherited method" dispatch error.
    stop(
      "calcSlopes() is not supported for BankMarket: the bank module consumes ",
      "pre-calibrated Stan-posterior parameters supplied to bank_fit(parameters=), ",
      "rather than calibrating demand slopes from margins. See ?bank_fit."
    )
  }
)

## ---------------------------------------------------------------------
## calcMC(): bank-level "everything but markup" implied-rate components,
## with liquidity pooling on merger (design/ARCHITECTURE.md's "Merger ...
## pools the combined entity's liquidity distribution").
## ---------------------------------------------------------------------

#' Pooled (or frozen) bank-level liquidity shadow cost
#'
#' Mirrors `simulate_merger_exposure.R`'s `pricing_map_exposure()` STEP 6
#' exactly: `lambda_L_curr`/`lambda_D_curr`/`sigma_W_curr` START as each
#' bank's own FROZEN baseline (`object@baseLambda_L`/`_D`/`baseSigma_W`,
#' the migrated `pre_liq` object -- computed once at `bank_fit()` time from
#' each bank's own pre-merger rows), and are OVERRIDDEN only for banks whose
#' post-merger owner differs from their pre-merger owner (the merging
#' parties) -- and ONLY when `dynamicLiquidity = TRUE`. Every other bank's
#' liquidity shadow cost is held fixed across all counterfactual scenarios,
#' exactly as the legacy script never recomputes `pre_liq` for competitor
#' banks. This is what makes `dynamicLiquidity = FALSE` (the
#' "Competition-only" scenario) and `dynamicLiquidity = TRUE` (the "Full
#' Merger" scenario) differ ONLY in the merging parties' liquidity state,
#' not in every bank's -- the correct decomposition per
#' `design/ARCHITECTURE.md`'s uncertainty-delta requirement.
#'
#' Asset-weighted psi for a merging group, using TOTAL bank assets
#'
#' Reproduces `simulate_merger_exposure.R`'s `psi_rule == "asset_weighted"`
#' branch: `weighted.mean(psi[g], w = pmax(assets[g], 1))`, where `assets` is
#' TOTAL Call-Report bank assets (`create_stability_data.R:275-276`). A
#' red-team review (2026-09) found the migration had substituted
#' `liquidAssets + equity` for this weight -- a materially different,
#' undocumented quantity (see the `assets` slot roxygen note on
#' `BankMarket-class`). This falls back to that proxy ONLY if the caller did
#' not supply `data$assets` to `bank_fit()`, and warns explicitly rather than
#' silently substituting.
#'
#' @keywords internal
#' @noRd
.bank_asset_weighted_psi <- function(object, g) {
  a <- object@assets[g]
  if (any(is.na(a))) {
    warning(
      "psiRule = 'asset_weighted' is using liquidAssets + equity as a proxy for ",
      "total bank assets because data$assets was not supplied to bank_fit(). ",
      "This does NOT match simulate_merger_exposure.R's true asset-weighted psi ",
      "rule. Supply data$assets for legacy-faithful behavior.",
      call. = FALSE
    )
    a <- object@liquidAssets[g] + object@equity[g]
  }
  stats::weighted.mean(object@psi[g], w = pmax(a, 1))
}

#' @keywords internal
#' @noRd
.bank_group_liquidity <- function(object, shares, weights, dynamicLiquidity, psiRule) {
  lambda_L <- object@baseLambda_L
  lambda_D <- object@baseLambda_D
  sigma_W <- object@baseSigma_W

  if (!dynamicLiquidity) {
    return(list(lambda_L = lambda_L, lambda_D = lambda_D, sigma_W = sigma_W))
  }

  ownerPost <- object@ownerPost
  ownerPre <- object@ownerPre
  ## Only banks whose owner actually changed (the merging parties) are
  ## pooled and recomputed; every other bank keeps its frozen baseline --
  ## matches legacy: only bank_a_id/bank_b_id's group is ever recomputed.
  mergedBankIdx <- which(ownerPost %in% ownerPost[ownerPre != ownerPost])
  if (length(mergedBankIdx) == 0) {
    return(list(lambda_L = lambda_L, lambda_D = lambda_D, sigma_W = sigma_W))
  }

  rowBank <- weights$rowBank
  groups <- split(mergedBankIdx, ownerPost[mergedBankIdx])
  for (g in groups) {
    rows <- which(rowBank %in% g)
    ## Reproduces simulate_merger_exposure.R's `liquidity_pressure_ab`:
    ##   calibrated_pressure[a] + calibrated_pressure[b]
    ##   + (expected_L_ab_at_current_rates - mean_L[a] - mean_L[b])
    ##   + (mean_D[a] + mean_D[b] - expected_D_ab_at_current_rates)
    expected_L_now <- sum(object@marketSize_L[rows] * shares$s_cond_L[rows] * shares$q1_L[rows])
    expected_D_now <- sum(object@marketSize_D[rows] * shares$s_cond_D[rows] * shares$q1_D[rows])
    liquidity_pressure_g <- sum(object@liquidityPressure[g]) +
      (expected_L_now - sum(object@baseMean_L[g])) +
      (sum(object@baseMean_D[g]) - expected_D_now)

    liq <- calc_portfolio_liquidity(
      market_size_L = object@marketSize_L[rows], market_size_D = object@marketSize_D[rows],
      s_cond_L = shares$s_cond_L[rows], s_cond_D = shares$s_cond_D[rows],
      outside_mu_L = shares$outside_mu_L[rows], outside_mu_D = shares$outside_mu_D[rows],
      phi_out_L = object@phiOut_L[rows], phi_out_D = object@phiOut_D[rows],
      equity = sum(object@equity[g]), liquid_assets = sum(object@liquidAssets[g]),
      liquidity_pressure = liquidity_pressure_g,
      psi = if (length(g) == 1L) {
        object@psi[g]
      } else {
        switch(psiRule,
          acquirer = object@psi[g[1]],
          target = object@psi[g[length(g)]],
          min = min(object@psi[g]),
          asset_weighted = .bank_asset_weighted_psi(object, g),
          stop("Invalid psiRule: ", psiRule)
        )
      },
      market_id = object@marketID[rows]
    )
    lambda_L[g] <- liq$lambda_liq_L
    lambda_D[g] <- liq$lambda_liq_D
    sigma_W[g] <- liq$sigma_W
  }
  list(lambda_L = lambda_L, lambda_D = lambda_D, sigma_W = sigma_W)
}

#' @rdname BankMarket-methods
#' @export
setMethod(
  f = "calcMC",
  signature = "BankMarket",
  definition = function(object, preMerger = TRUE, dynamicLiquidity = FALSE, ...) {
    margins <- calcMargins(object, preMerger = preMerger)

    if (preMerger) {
      lambda_L <- object@baseLambda_L
      lambda_D <- object@baseLambda_D
      sigma_W <- object@baseSigma_W
    } else {
      liq <- .bank_group_liquidity(object, margins$shares, margins$weights,
        dynamicLiquidity = dynamicLiquidity, psiRule = object@psiRule)
      lambda_L <- liq$lambda_L
      lambda_D <- liq$lambda_D
      sigma_W <- liq$sigma_W
    }

    rowBank <- margins$weights$rowBank
    nBanks <- length(object@labels)
    lev <- factor(rowBank, levels = seq_len(nBanks))
    cost_L <- as.numeric(tapply(margins$weights$pi_L * object@costRaw_L, lev, sum))
    cost_D <- as.numeric(tapply(margins$weights$pi_D * object@costRaw_D, lev, sum))
    r_f_L <- as.numeric(tapply(margins$weights$pi_L * object@riskFreeRate, lev, sum))
    r_f_D <- as.numeric(tapply(margins$weights$pi_D * object@riskFreeRate, lev, sum))
    cost_L[is.na(cost_L)] <- 0
    cost_D[is.na(cost_D)] <- 0
    r_f_L[is.na(r_f_L)] <- 0
    r_f_D[is.na(r_f_D)] <- 0

    list(
      mc_L = r_f_L + cost_L + lambda_L, mc_D = r_f_D + cost_D - lambda_D,
      lambda_L = lambda_L, lambda_D = lambda_D, sigma_W = sigma_W,
      cost_L = cost_L, cost_D = cost_D, r_f_L = r_f_L, r_f_D = r_f_D,
      markup_L = margins$markup_L, markdown_D = margins$markdown_D
    )
  }
)

## ---------------------------------------------------------------------
## calcPrices(): damped fixed-point solver + nleqslv Broyden fallback.
## Parity migration of solve_equilibrium_exposure() (SHA cc95e2e0). ONLY
## used for preMerger = FALSE -- preMerger = TRUE is observed data, not
## solved (see the "Key structural fact preserved" roxygen note above).
## ---------------------------------------------------------------------

#' @rdname BankMarket-methods
#' @export
setMethod(
  f = "calcPrices",
  signature = "BankMarket",
  definition = function(object, preMerger = TRUE, dynamicLiquidity = FALSE, ...) {
    if (preMerger) {
      return(object@pricePre)
    }

    nBanks <- length(object@labels)
    reopt <- which(object@reoptimizing)
    loanUpper <- object@loanRateUpper
    depUpper <- object@depositRateUpper
    ctrl <- object@control.equ
    damping <- ctrl$damping %||% 0.5
    tol <- ctrl$tol %||% 1e-6
    maxIter <- ctrl$max_iter %||% 250

    implied_once <- function(r_L_bank, r_D_bank) {
      obj2 <- object
      obj2@pricePost <- c(r_L_bank, r_D_bank)

      margins <- calcMargins(obj2, preMerger = FALSE)
      mc <- calcMC(obj2, preMerger = FALSE, dynamicLiquidity = dynamicLiquidity)

      r_L_new <- r_L_bank
      r_D_new <- r_D_bank
      r_L_new[reopt] <- mc$r_f_L[reopt] + margins$markup_L[reopt] + mc$cost_L[reopt] +
        mc$lambda_L[reopt] + object@omega_L[reopt]
      r_D_new[reopt] <- mc$r_f_D[reopt] - margins$markdown_D[reopt] + mc$cost_D[reopt] -
        mc$lambda_D[reopt] + object@omega_D[reopt]
      r_L_new[reopt] <- pmax(pmin(r_L_new[reopt], loanUpper), 0)
      r_D_new[reopt] <- pmax(pmin(r_D_new[reopt], depUpper), 0)
      list(r_L = r_L_new, r_D = r_D_new)
    }

    nBanksTot <- length(object@labels)
    r_L_curr <- object@pricePre[seq_len(nBanksTot)]
    r_D_curr <- object@pricePre[nBanksTot + seq_len(nBanksTot)]

    converged <- FALSE
    for (iter in seq_len(maxIter)) {
      implied <- implied_once(r_L_curr, r_D_curr)
      diff <- max(abs(r_L_curr[reopt] - implied$r_L[reopt])) +
        max(abs(r_D_curr[reopt] - implied$r_D[reopt]))
      if (is.na(diff)) break
      r_L_curr[reopt] <- (1 - damping) * r_L_curr[reopt] + damping * implied$r_L[reopt]
      r_D_curr[reopt] <- (1 - damping) * r_D_curr[reopt] + damping * implied$r_D[reopt]
      r_L_curr[reopt] <- pmax(pmin(r_L_curr[reopt], loanUpper), 0)
      r_D_curr[reopt] <- pmax(pmin(r_D_curr[reopt], depUpper), 0)
      if (diff < tol) {
        converged <- TRUE
        break
      }
    }

    if (!converged) {
      K <- length(reopt)
      logit_L <- stats::qlogis(pmax(pmin(r_L_curr[reopt] / loanUpper, 0.9999), 0.0001))
      logit_D <- stats::qlogis(pmax(pmin(r_D_curr[reopt] / depUpper, 0.9999), 0.0001))
      x0 <- c(logit_L, logit_D)

      residual_fn <- function(x) {
        x_L <- x[1:K]
        x_D <- x[(K + 1):(2 * K)]
        r_L_active <- loanUpper * stats::plogis(x_L)
        r_D_active <- depUpper * stats::plogis(x_D)
        r_L_full <- r_L_curr
        r_D_full <- r_D_curr
        r_L_full[reopt] <- r_L_active
        r_D_full[reopt] <- r_D_active
        implied <- implied_once(r_L_full, r_D_full)
        c(10000 * (implied$r_L[reopt] - r_L_active), 10000 * (implied$r_D[reopt] - r_D_active))
      }

      res <- nleqslv::nleqslv(x0, residual_fn,
        control = list(xtol = tol, ftol = tol, maxit = maxIter), method = "Broyden"
      )
      if (res$termcd <= 2) {
        final_x_L <- res$x[1:K]
        final_x_D <- res$x[(K + 1):(2 * K)]
        r_L_curr[reopt] <- loanUpper * stats::plogis(final_x_L)
        r_D_curr[reopt] <- depUpper * stats::plogis(final_x_D)
      } else {
        warning("BankMarket calcPrices(): neither the damped fixed-point solver ",
          "nor the nleqslv Broyden fallback converged (nleqslv termcd = ", res$termcd, ").")
      }
    }

    price <- c(r_L_curr, r_D_curr)
    names(price) <- c(paste0(object@labels, ":L"), paste0(object@labels, ":D"))
    price
  }
)
