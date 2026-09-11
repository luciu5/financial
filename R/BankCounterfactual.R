#' Bank merger counterfactual: constructor, posterior mapping, uncertainty deltas
#'
#' `bank_fit()` is `financial`'s own constructor (per
#' `design/ARCHITECTURE.md`: "`financial` ships its own constructors
#' `debt_fit()`/`bank_fit()`") for the `BankFit` `FinancialFit` subclass. It
#' implements the two-argument `bank_fit(parameters =, data =)` contract from
#' `design/STAN-BANKFIT-MAPPING.md`: `parameters` supplies (one or more)
#' Stan posterior draws of the 15-parameter minimal sufficient set;
#' `data` supplies everything the deterministic solver needs that is NOT in
#' the posterior (design matrices, market/bank identifiers, balance-sheet
#' observables), mirroring legacy `load_merger_inputs()`
#' (`merger_common.R:5-19`).
#'
#' `simulate(BankFit, ownerPost = ...)` runs the merger counterfactual
#' (parity migration of `simulate_merger_exposure.R`'s `STEP 5`: solve
#' `"Competition-only"` and `"Full Merger"` post-merger scenarios). For a
#' multi-draw posterior, it MAPS OVER every draw and returns posterior
#' summaries (mean/sd/q05/q50/q95, plus `posterior_prob_positive`/
#' `posterior_odds_positive` for scenario-DIFFERENCE quantities) per
#' `design/STAN-BANKFIT-MAPPING.md`'s aggregation recipe
#' (`gather_merger_results.R:119-188`). A single draw / point estimate
#' returns the point result (no posterior summary columns) -- this is the
#' "single-draw == point-estimate round trip" required by the migration
#' brief, and holds by construction since `bank_fit()` and `simulate()` do
#' not branch on draw count until the AGGREGATION step.
#'
#' @name BankFit-lifecycle
NULL

## ---------------------------------------------------------------------
## bank_fit(): the two-argument Stan-posterior-ingestion constructor.
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.bank_align <- function(x, labels, what) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.null(names(x))) {
    missing <- setdiff(labels, names(x))
    if (length(missing) > 0) {
      stop("'", what, "' is missing entries for bank(s): ", paste(missing, collapse = ", "))
    }
    return(x[labels])
  }
  if (length(x) != length(labels)) {
    stop("'", what, "' must either be named (by bank id) or have length ",
      length(labels), " (== number of unique banks), in sort(unique(bank_id)) order.")
  }
  stats::setNames(x, labels)
}

#' Build one `BankMarket` from a single parameter draw + structural data
#'
#' Parity migration of `simulate_merger_exposure.R`'s `STEP 1-3` (load
#' inputs, derive Stan primitives, calibrate baseline) for ONE posterior
#' draw. Called once per draw by [bank_fit()].
#'
#' @param draw A named numeric vector (one Stan posterior draw / point
#'   estimate).
#' @param data The two-argument `bank_fit(data = ...)` structural-data list;
#'   see [bank_fit()] for the full field contract.
#' @return A calibrated `BankMarket` object with `pricePre` set to the
#'   bank-level observed footprint-weighted baseline rates, and
#'   `omega_L`/`omega_D`/`baseLambda_L`/`baseLambda_D`/`baseSigma_W`/
#'   `baseMean_L`/`baseMean_D`/`liquidityPressure` calibrated per bank.
#' @keywords internal
#' @noRd
.bank_market_from_draw <- function(draw, data) {
  bank_id <- as.character(data$bank_id)
  market_id <- as.character(data$market_id)
  labels <- sort(unique(bank_id))
  nBanks <- length(labels)
  n <- length(bank_id)

  ## Renormalize baseline conditional shares to sum to 1 within market
  ## (matches simulate_merger_exposure.R's df_year %>% group_by(market_id) %>%
  ## mutate(s_cond_L = s_cond_L / sum(s_cond_L), ...)).
  s_cond_L0 <- data$s_cond_L / stats::ave(data$s_cond_L, market_id, FUN = sum)
  s_cond_D0 <- data$s_cond_D / stats::ave(data$s_cond_D, market_id, FUN = sum)

  prim <- derive_bank_primitives(draw, list(
    H_out_L = data$H_out_L, H_out_D = data$H_out_D,
    out_iv_L = data$out_iv_L, out_iv_D = data$out_iv_D,
    market_state_region_id = data$market_state_region_id,
    rho_out_L = data$rho_out_L, rho_out_D = data$rho_out_D
  ))

  K_cost_L <- ncol(data$W_L)
  K_cost_D <- ncol(data$W_D)
  eta_L <- if (K_cost_L > 0) bank_param_vector(draw, "eta_L", K_cost_L) else numeric(0)
  eta_D <- if (K_cost_D > 0) bank_param_vector(draw, "eta_D", K_cost_D) else numeric(0)
  cost_raw_L <- if (K_cost_L > 0) as.numeric(data$W_L %*% eta_L) else rep(0, n)
  cost_raw_D <- if (K_cost_D > 0) as.numeric(data$W_D %*% eta_D) else rep(0, n)

  group_id <- as.character(data$group_id %||% bank_id)
  groupLevels <- sort(unique(group_id))
  gamma_psi <- bank_param_vector(draw, "gamma_psi", ncol(data$Z_psi))
  psiByGroup <- calc_bank_psi(data$Z_psi, gamma_psi)
  names(psiByGroup) <- groupLevels
  ## one liquidity-friction group per bank, per this row's group -> first
  ## occurrence (matches simulate_merger_exposure.R's
  ## `Z_psi[df_year$group_id[rows_b[1]], ]` -- the group is constant within
  ## a bank in the legacy panel; we take the first observed group per bank).
  bankGroup <- vapply(labels, function(b) group_id[match(b, bank_id)], character(1))
  psiBank <- psiByGroup[bankGroup]

  equity <- .bank_align(data$equity, labels, "equity")
  liquidAssets <- .bank_align(data$liquid_assets, labels, "liquid_assets")
  ownerPre <- as.character(.bank_align(data$ownerPre %||% stats::setNames(labels, labels), labels, "ownerPre"))
  ownerPost <- as.character(.bank_align(data$ownerPost %||% ownerPre, labels, "ownerPost"))
  reoptimizing <- as.logical(.bank_align(data$reoptimizing %||% stats::setNames(rep(TRUE, nBanks), labels), labels, "reoptimizing"))
  liquidityPressureIn <- data$liquidity_pressure
  if (!is.null(liquidityPressureIn)) liquidityPressureIn <- .bank_align(liquidityPressureIn, labels, "liquidity_pressure")

  ## Antitrust's own validity method warns "'ownerPost' and 'ownerPre' are
  ## the same" whenever they're identical -- expected and benign here, since
  ## bank_fit() builds the CALIBRATED baseline before any merger is
  ## specified (data$ownerPost defaults to data$ownerPre, i.e. "no merger
  ## yet"); simulate(BankFit, ownerPost = ...) sets a real post-merger
  ## ownership afterwards. Suppressed to avoid noise on every bank_fit()
  ## call; a genuinely mismatched ownerPre/ownerPost still validates
  ## normally (no other validity checks are suppressed).
  prelim <- suppressWarnings(new("BankMarket",
    labels = labels, ownerPre = ownerPre, ownerPost = ownerPost,
    marketID = market_id, bankRow = bank_id,
    marketSize_L = data$market_size_L, marketSize_D = data$market_size_D,
    sCondBase_L = s_cond_L0, sCondBase_D = s_cond_D0,
    rBase_L = data$r_L, rBase_D = data$r_D,
    alpha_L = prim$alpha_L, alpha_D = prim$alpha_D,
    outsideMu_L = prim$outside_mu_L, outsideMu_D = prim$outside_mu_D,
    phiOut_L = prim$phi_out_L, phiOut_D = prim$phi_out_D,
    costRaw_L = cost_raw_L, costRaw_D = cost_raw_D,
    riskFreeRate = data$r_f,
    equity = as.numeric(equity), liquidAssets = as.numeric(liquidAssets),
    psi = as.numeric(psiBank),
    omega_L = rep(0, nBanks), omega_D = rep(0, nBanks),
    liquidityPressure = rep(0, nBanks),
    baseLambda_L = rep(0, nBanks), baseLambda_D = rep(0, nBanks), baseSigma_W = rep(0, nBanks),
    baseMean_L = rep(0, nBanks), baseMean_D = rep(0, nBanks),
    reoptimizing = reoptimizing,
    loanRateUpper = data$loanRateUpper %||% 0.25,
    depositRateUpper = data$depositRateUpper %||% 0.20,
    psiRule = data$psiRule %||% "asset_weighted",
    control.equ = data$control.equ %||% list(damping = 0.5, tol = 1e-6, max_iter = 250)
  ))

  sharesBase <- .bank_shares_baseline(prelim)
  weights <- .bank_quantity_weights(prelim, sharesBase)
  rowBank <- weights$rowBank
  lev <- factor(rowBank, levels = seq_len(nBanks))

  r_L_bank_pre <- as.numeric(tapply(weights$pi_L * data$r_L, lev, sum))
  r_D_bank_pre <- as.numeric(tapply(weights$pi_D * data$r_D, lev, sum))
  r_L_bank_pre[is.na(r_L_bank_pre)] <- 0
  r_D_bank_pre[is.na(r_D_bank_pre)] <- 0

  ## Pre-merger markups/markdowns at baseline shares (calcMargins(preMerger=TRUE)
  ## does not depend on pricePre being set -- see BankEquilibrium.R).
  marginsPre <- calcMargins(prelim, preMerger = TRUE)
  mcPre0 <- calcMC(prelim, preMerger = TRUE) # cost_L/cost_D/r_f_L/r_f_D only; lambda still 0

  ## Raw (uncalibrated) baseline liquidity per bank, using caller-supplied
  ## liquidity_pressure if given, else calc_portfolio_liquidity()'s own
  ## mean_L - mean_D - equity default (mirrors pre_liq_raw in the legacy
  ## script).
  preLiqRaw <- lapply(seq_len(nBanks), function(b) {
    rows <- which(rowBank == b)
    calc_portfolio_liquidity(
      market_size_L = prelim@marketSize_L[rows], market_size_D = prelim@marketSize_D[rows],
      s_cond_L = sharesBase$s_cond_L[rows], s_cond_D = sharesBase$s_cond_D[rows],
      outside_mu_L = sharesBase$outside_mu_L[rows], outside_mu_D = sharesBase$outside_mu_D[rows],
      phi_out_L = prelim@phiOut_L[rows], phi_out_D = prelim@phiOut_D[rows],
      equity = equity[b], liquid_assets = liquidAssets[b],
      liquidity_pressure = if (!is.null(liquidityPressureIn)) liquidityPressureIn[b] else NULL,
      psi = prelim@psi[b], market_id = prelim@marketID[rows]
    )
  })

  liquidityPressureBank <- vapply(preLiqRaw, function(x) x$liquidity_pressure, numeric(1))

  for (b in which(reoptimizing)) {
    target_lambda_L <- r_L_bank_pre[b] - mcPre0$r_f_L[b] - marginsPre$markup_L[b] - mcPre0$cost_L[b]
    target_lambda_D <- mcPre0$r_f_D[b] - marginsPre$markdown_D[b] + mcPre0$cost_D[b] - r_D_bank_pre[b]
    liquidityPressureBank[b] <- calibrate_liquidity_pressure_from_wedges(
      liq = preLiqRaw[[b]], target_lambda_L = target_lambda_L, target_lambda_D = target_lambda_D,
      psi = prelim@psi[b], liquid_assets = liquidAssets[b]
    )
  }

  preLiq <- lapply(seq_len(nBanks), function(b) {
    rows <- which(rowBank == b)
    calc_portfolio_liquidity(
      market_size_L = prelim@marketSize_L[rows], market_size_D = prelim@marketSize_D[rows],
      s_cond_L = sharesBase$s_cond_L[rows], s_cond_D = sharesBase$s_cond_D[rows],
      outside_mu_L = sharesBase$outside_mu_L[rows], outside_mu_D = sharesBase$outside_mu_D[rows],
      phi_out_L = prelim@phiOut_L[rows], phi_out_D = prelim@phiOut_D[rows],
      equity = equity[b], liquid_assets = liquidAssets[b],
      liquidity_pressure = liquidityPressureBank[b],
      psi = prelim@psi[b], market_id = prelim@marketID[rows]
    )
  })

  baseLambda_L <- vapply(preLiq, function(x) x$lambda_liq_L, numeric(1))
  baseLambda_D <- vapply(preLiq, function(x) x$lambda_liq_D, numeric(1))
  baseSigma_W <- vapply(preLiq, function(x) x$sigma_W, numeric(1))
  baseMean_L <- vapply(preLiq, function(x) x$mean_L, numeric(1))
  baseMean_D <- vapply(preLiq, function(x) x$mean_D, numeric(1))

  omega_L <- rep(0, nBanks)
  omega_D <- rep(0, nBanks)
  for (b in which(reoptimizing)) {
    omega_L[b] <- r_L_bank_pre[b] - mcPre0$r_f_L[b] - marginsPre$markup_L[b] - mcPre0$cost_L[b] - baseLambda_L[b]
    omega_D[b] <- r_D_bank_pre[b] - mcPre0$r_f_D[b] + marginsPre$markdown_D[b] - mcPre0$cost_D[b] + baseLambda_D[b]
  }

  prelim@omega_L <- omega_L
  prelim@omega_D <- omega_D
  prelim@liquidityPressure <- liquidityPressureBank
  prelim@baseLambda_L <- baseLambda_L
  prelim@baseLambda_D <- baseLambda_D
  prelim@baseSigma_W <- baseSigma_W
  prelim@baseMean_L <- baseMean_L
  prelim@baseMean_D <- baseMean_D
  prelim@pricePre <- c(r_L_bank_pre, r_D_bank_pre)
  names(prelim@pricePre) <- c(paste0(labels, ":L"), paste0(labels, ":D"))
  prelim@mcPre <- c(mcPre0$r_f_L + mcPre0$cost_L + baseLambda_L, mcPre0$r_f_D + mcPre0$cost_D - baseLambda_D)

  prelim
}

#' Construct a `FinancialFit`-compatible bank fit from Stan posterior draws
#'
#' Two-argument constructor per `design/STAN-BANKFIT-MAPPING.md`:
#' `parameters` supplies the 15-parameter minimal sufficient Stan draw set
#' (`alpha_bar_L`, `alpha_bar_D`, `sigma_alpha_state_L`,
#' `sigma_alpha_state_D`, `alpha_state_raw_L[1..R_state]`,
#' `alpha_state_raw_D[1..R_state]`, `theta_out_L[1..K_out_L]`,
#' `theta_out_D[1..K_out_D]`, `alpha_out_L`, `alpha_out_D`, `log_phi_bar_L`,
#' `log_phi_bar_D`, `eta_L[1..K_cost]`, `eta_D[1..K_cost]`,
#' `gamma_psi[1..K_psi]`) as EITHER a single named numeric vector (point
#' estimate or one draw) or a matrix/data.frame of `N_draws` rows (columns
#' named with either the `name[i]` or `name.i` convention -- both accepted,
#' see [normalize_bank_draws()]); `data` supplies everything the
#' deterministic solver needs that is NOT in the posterior.
#'
#' @param parameters See [normalize_bank_draws()] for the accepted shapes.
#' @param data A list with the following elements (row-level fields have
#'   one entry per bank-market observation; bank-level fields have one
#'   entry per unique bank, either named by bank id or given in
#'   `sort(unique(bank_id))` order):
#'   \describe{
#'     \item{`bank_id`, `market_id`}{Row-level bank/market identifiers.}
#'     \item{`s_cond_L`, `s_cond_D`}{Row-level baseline conditional
#'       loan/deposit shares (renormalized to sum to 1 within market).}
#'     \item{`market_size_L`, `market_size_D`}{Row-level market size.}
#'     \item{`r_L`, `r_D`}{Row-level observed baseline loan/deposit rates.}
#'     \item{`r_f`}{Row-level risk-free rate.}
#'     \item{`W_L`, `W_D`}{Row-level cost-shifter design matrices (`N x
#'       K_cost`; `K_cost` may be 0, i.e. zero columns).}
#'     \item{`H_out_L`, `H_out_D`}{Row-level outside-share design matrices
#'       (`N x K_out_{L,D}`) -- see `derive_bank_primitives()`'s roxygen note
#'       on the row-level (vs. legacy deduplicated `out_id`) granularity.}
#'     \item{`out_iv_L`, `out_iv_D`}{Row-level outside instrumental
#'       variable.}
#'     \item{`market_state_region_id`}{Row-level integer index into
#'       `alpha_state_raw_{L,D}`.}
#'     \item{`rho_out_L`, `rho_out_D`}{Optional scalars, default `1`.}
#'     \item{`group_id`}{Optional row-level liquidity-friction group id
#'       (default: `bank_id`, i.e. one friction group per bank -- see
#'       `BankBalanceSheet.R`'s roxygen note on the Z_psi-without-original-
#'       panel resolution). Must be constant within each bank.}
#'     \item{`Z_psi`}{A `G x K_psi` design matrix, one row per distinct
#'       `group_id`, in `sort(unique(group_id))` order.}
#'     \item{`equity`, `liquid_assets`}{Bank-level.}
#'     \item{`liquidity_pressure`}{Optional bank-level override for the mean
#'       liquidity-stress state; default `NULL` uses
#'       `calc_portfolio_liquidity()`'s own default.}
#'     \item{`ownerPre`, `ownerPost`}{Optional bank-level ownership vectors;
#'       default `ownerPre` = each bank's own id (no pre-existing common
#'       ownership) and `ownerPost = ownerPre` (no merger) -- callers
#'       typically override `ownerPost` in [simulate()] instead of here.}
#'     \item{`reoptimizing`}{Optional bank-level logical; default all
#'       `TRUE.`}
#'     \item{`loanRateUpper`, `depositRateUpper`, `psiRule`,
#'       `control.equ`}{Optional solver controls.}
#'   }
#' @return A `BankFit` object. `@model` holds the `BankMarket` built from the
#'   FIRST draw (or the point estimate); `@diagnostics$draws` holds every
#'   draw's `BankMarket` (length 1 for a point estimate/single draw).
#' @export
bank_fit <- function(parameters, data) {
  draws_mat <- normalize_bank_draws(parameters)
  nDraws <- nrow(draws_mat)

  models <- vector("list", nDraws)
  for (i in seq_len(nDraws)) {
    models[[i]] <- .bank_market_from_draw(bank_draw_row(draws_mat, i), data)
  }

  new("BankFit",
    spec = list(family = "bank", conduct = "nash-in-rates", liquidity = "beta-shortfall"),
    model = models[[1]],
    parameters = list(n_draws = nDraws, draw_names = colnames(draws_mat)),
    observed = list(
      bank_id = data$bank_id, market_id = data$market_id,
      labels = models[[1]]@labels
    ),
    diagnostics = list(
      status = "completed", model_class = "BankMarket",
      n_draws = nDraws, draws = models
    )
  )
}

## ---------------------------------------------------------------------
## simulate(): the merger counterfactual, mapped over posterior draws.
## ---------------------------------------------------------------------

#' Solve one draw's merger counterfactual (both decomposition scenarios)
#'
#' Parity migration of `simulate_merger_exposure.R`'s `STEP 5`: solves the
#' `"Competition-only"` (`dynamicLiquidity = FALSE`) and `"Full Merger"`
#' (`dynamicLiquidity = TRUE`) post-merger scenarios under the SAME
#' `ownerPost`, and reports the uncertainty-delta outputs required by
#' `design/ARCHITECTURE.md`'s "Counterfactual uncertainty response" (default
#' exposure-only path): `Delta Var(pooled shortfall W-tilde)` (`sigma_W^2`
#' pre vs. post), `Delta shadow-cost Lambda`, and `Delta E[Gamma]` (the
#' diversification effect of pooling -- proxied here by the change in the
#' expected liquidity penalty `E[Gamma] = expected_liq_penalty`, i.e. the
#' quadratic liquidity cost itself, since `stability` does not separately
#' name a `Gamma` object -- `Lambda` IS `Gamma`'s marginal price and
#' `expected_liq_penalty` IS `E[Gamma]`; see `BankRisk.R`'s
#' `calc_portfolio_liquidity()` docs).
#'
#' @keywords internal
#' @noRd
.bank_simulate_one_draw <- function(model, ownerPost, dynamicLiquidity_compare = TRUE) {
  labels <- model@labels
  if (!is.null(ownerPost)) {
    model@ownerPost <- .bank_align(ownerPost, labels, "ownerPost")
  }

  compSol <- model
  compSol@pricePost <- calcPrices(compSol, preMerger = FALSE, dynamicLiquidity = FALSE)
  compMc <- calcMC(compSol, preMerger = FALSE, dynamicLiquidity = FALSE)

  fullSol <- model
  fullSol@pricePost <- calcPrices(fullSol, preMerger = FALSE, dynamicLiquidity = TRUE)
  fullMc <- calcMC(fullSol, preMerger = FALSE, dynamicLiquidity = TRUE)

  nBanks <- length(labels)
  r_L_pre <- model@pricePre[seq_len(nBanks)]
  r_D_pre <- model@pricePre[nBanks + seq_len(nBanks)]
  r_L_comp <- compSol@pricePost[seq_len(nBanks)]
  r_D_comp <- compSol@pricePost[nBanks + seq_len(nBanks)]
  r_L_full <- fullSol@pricePost[seq_len(nBanks)]
  r_D_full <- fullSol@pricePost[nBanks + seq_len(nBanks)]

  changed <- which(model@ownerPost %in% model@ownerPost[model@ownerPre != model@ownerPost])

  ## Uncertainty-delta outputs (first-class, per design/ARCHITECTURE.md):
  ## computed only for the banks whose ownership/liquidity actually pooled.
  delta_var_W <- fullMc$sigma_W[changed]^2 - model@baseSigma_W[changed]^2
  delta_lambda <- 0.5 * (fullMc$lambda_L[changed] + fullMc$lambda_D[changed]) -
    0.5 * (model@baseLambda_L[changed] + model@baseLambda_D[changed])
  ## E[Gamma] proxy = the expected liquidity penalty itself (0.5*psi*E[shortfall^2]);
  ## recovered analytically from lambda via the same tail_probability-weighted
  ## relationship calc_portfolio_liquidity() uses is not invertible in general,
  ## so we recompute expected_liq_penalty directly per bank group for the delta.
  gamma_pre <- vapply(changed, function(b) {
    rows <- which(model@bankRow == labels[b])
    calc_portfolio_liquidity(
      market_size_L = model@marketSize_L[rows], market_size_D = model@marketSize_D[rows],
      s_cond_L = model@sCondBase_L[rows], s_cond_D = model@sCondBase_D[rows],
      outside_mu_L = model@outsideMu_L[rows], outside_mu_D = model@outsideMu_D[rows],
      phi_out_L = model@phiOut_L[rows], phi_out_D = model@phiOut_D[rows],
      equity = model@equity[b], liquid_assets = model@liquidAssets[b],
      liquidity_pressure = model@liquidityPressure[b], psi = model@psi[b],
      market_id = model@marketID[rows]
    )$expected_liq_penalty
  }, numeric(1))

  sharesFull <- calcShares(fullSol, preMerger = FALSE)
  weightsFull <- .bank_quantity_weights(fullSol, sharesFull)
  gamma_post <- vapply(changed, function(b) {
    g <- model@ownerPost[b]
    g_members <- which(model@ownerPost == g)
    rows <- which(weightsFull$rowBank %in% g_members)
    expected_L_now <- sum(fullSol@marketSize_L[rows] * sharesFull$s_cond_L[rows] * sharesFull$q1_L[rows])
    expected_D_now <- sum(fullSol@marketSize_D[rows] * sharesFull$s_cond_D[rows] * sharesFull$q1_D[rows])
    pressure <- sum(model@liquidityPressure[g_members]) +
      (expected_L_now - sum(model@baseMean_L[g_members])) +
      (sum(model@baseMean_D[g_members]) - expected_D_now)
    psi_g <- if (length(g_members) == 1L) {
      model@psi[g_members]
    } else {
      switch(model@psiRule,
        acquirer = model@psi[g_members[1]], target = model@psi[g_members[length(g_members)]],
        min = min(model@psi[g_members]),
        asset_weighted = stats::weighted.mean(model@psi[g_members], w = pmax(model@liquidAssets[g_members] + model@equity[g_members], 1)),
        stop("Invalid psiRule: ", model@psiRule)
      )
    }
    calc_portfolio_liquidity(
      market_size_L = fullSol@marketSize_L[rows], market_size_D = fullSol@marketSize_D[rows],
      s_cond_L = sharesFull$s_cond_L[rows], s_cond_D = sharesFull$s_cond_D[rows],
      outside_mu_L = sharesFull$outside_mu_L[rows], outside_mu_D = sharesFull$outside_mu_D[rows],
      phi_out_L = fullSol@phiOut_L[rows], phi_out_D = fullSol@phiOut_D[rows],
      equity = sum(model@equity[g_members]), liquid_assets = sum(model@liquidAssets[g_members]),
      liquidity_pressure = pressure, psi = psi_g, market_id = fullSol@marketID[rows]
    )$expected_liq_penalty
  }, numeric(1))

  list(
    labels = labels,
    r_L_pre = stats::setNames(r_L_pre, labels), r_D_pre = stats::setNames(r_D_pre, labels),
    r_L_comp = stats::setNames(r_L_comp, labels), r_D_comp = stats::setNames(r_D_comp, labels),
    r_L_full = stats::setNames(r_L_full, labels), r_D_full = stats::setNames(r_D_full, labels),
    lambda_comp = stats::setNames(0.5 * (compMc$lambda_L + compMc$lambda_D), labels),
    lambda_full = stats::setNames(0.5 * (fullMc$lambda_L + fullMc$lambda_D), labels),
    sigma_W_comp = stats::setNames(compMc$sigma_W, labels),
    sigma_W_full = stats::setNames(fullMc$sigma_W, labels),
    changed_banks = labels[changed],
    delta_var_W = stats::setNames(delta_var_W, labels[changed]),
    delta_lambda = stats::setNames(delta_lambda, labels[changed]),
    delta_E_gamma = stats::setNames(gamma_post - gamma_pre, labels[changed])
  )
}

#' Aggregate a per-quantity vector of draws into the posterior summary recipe
#'
#' Per `design/STAN-BANKFIT-MAPPING.md`'s "Aggregation recipe"
#' (`gather_merger_results.R:119-188`): `mean`, `sd`, `q05`, `q50`, `q95`,
#' tagged with `n_draws`; if `scenario_diff = TRUE` (this quantity IS a
#' scenario-difference quantity, e.g. Full-Merger minus Competition-only),
#' additionally `posterior_prob_positive = mean(x > 0)` and
#' `posterior_odds_positive`.
#'
#' @keywords internal
#' @noRd
.bank_aggregate_draws <- function(x, scenario_diff = FALSE) {
  n <- length(x)
  if (n <= 1L) {
    ## Single draw / point estimate: return the point value directly (the
    ## "single-draw == point-estimate round trip" -- no posterior summary
    ## columns are meaningful for a degenerate 1-draw posterior).
    return(list(value = if (n == 1L) x[[1]] else NA_real_, n_draws = n))
  }
  xv <- unlist(x)
  out <- list(
    mean = mean(xv, na.rm = TRUE), sd = stats::sd(xv, na.rm = TRUE),
    q05 = as.numeric(stats::quantile(xv, 0.05, na.rm = TRUE, names = FALSE)),
    q50 = as.numeric(stats::quantile(xv, 0.50, na.rm = TRUE, names = FALSE)),
    q95 = as.numeric(stats::quantile(xv, 0.95, na.rm = TRUE, names = FALSE)),
    n_draws = n
  )
  if (scenario_diff) {
    prob <- mean(xv > 0, na.rm = TRUE)
    out$posterior_prob_positive <- prob
    out$posterior_odds_positive <- if (prob <= 0) 0 else if (prob >= 1) Inf else prob / (1 - prob)
  }
  out
}

#' `simulate()`/`simulate_steps()`/`respecify()` for `BankFit`
#'
#' `simulate(BankFit, ownerPost, ...)` runs the exposure-based merger
#' counterfactual for EVERY draw in `object@diagnostics$draws`, then
#' aggregates per-quantity results using [`.bank_aggregate_draws()`]. This is
#' the exposure-only counterfactual path described in
#' `design/ARCHITECTURE.md`'s "Counterfactual uncertainty response":
#' ownership/footprint change; the Beta participation-share law
#' (`outsideMu_{L,D}`/`phiOut_{L,D}`) is held fixed unless a caller-supplied
#' `uncertainty` primitive-shift step is provided (NOT implemented in this
#' migration wave -- see the roxygen note below and the migration report).
#'
#' `simulate_steps()` and `respecify()` are NOT implemented for `BankFit` in
#' this migration wave, for the same reason given for `DebtFit`
#' (`DebtCounterfactual.R`): sequential multi-step `CounterfactualPath`
#' resume is a real feature called for by `design/ARCHITECTURE.md`'s
#' "Counterfactual uncertainty response" opt-in primitive-shift path, but is
#' explicitly out of scope for a parity migration of the existing
#' single-step legacy economics (`simulate_merger_exposure.R` itself only
#' ever solves ONE merger). They fail loudly per the `FinancialFit`
#' default-reject contract (inherited unchanged from
#' `FinancialArchitecture.R`) rather than silently no-oping.
#'
#' @section Designed seam for the opt-in primitive-shift path (NOT implemented):
#' Per `design/ARCHITECTURE.md`: "financial defines its OWN counterfactual
#' step type ... with an OPTIONAL uncertainty field: a user-supplied
#' merger-induced change to the Beta primitive (bank: participation
#' precision phi / shareOutParm)." The seam left here for that future work:
#' `.bank_simulate_one_draw()` reads `model@outsideMu_{L,D}`/`phiOut_{L,D}`
#' UNCHANGED from the calibrated draw for every counterfactual scenario --
#' a future `uncertainty` argument would only need to override those two
#' slots on `model` (e.g. `model@phiOut_L[changed_rows] <- new_phi`) before
#' calling `calcPrices()`/`calcMC()`; nothing else in the solver would need
#' to change. When absent (as here), behavior is identical to the
#' exposure-only default, so adding that seam later cannot perturb this
#' migration's parity tests.
#'
#' @param object A `BankFit`.
#' @param ownerPost A post-merger bank-level ownership vector (named by bank
#'   id, or in `sort(unique(bank_id))` order) or `NULL` to reuse the
#'   currently fitted post-merger ownership.
#' @param ... Currently unused.
#' @return An updated `BankFit`. `@diagnostics$counterfactual` holds the
#'   full per-draw and aggregated result (see the roxygen details above);
#'   `@model@pricePost` is set from the FIRST draw's "Full Merger" solution
#'   for convenience.
#' @name BankFit-lifecycle
#' @rdname BankFit-lifecycle
#' @export
setMethod("simulate", "BankFit", function(object, ownerPost = NULL, ...) {
  draws <- object@diagnostics$draws
  results <- lapply(draws, .bank_simulate_one_draw, ownerPost = ownerPost)

  nDraws <- length(results)
  labels <- results[[1]]$labels

  perBank <- function(field) {
    stats::setNames(lapply(labels, function(b) {
      vals <- lapply(results, function(r) {
        val <- r[[field]][b]
        if (is.na(val)) NA_real_ else as.numeric(val)
      })
      .bank_aggregate_draws(vals, scenario_diff = FALSE)
    }), labels)
  }

  perBank_diff <- function(field_full, field_comp) {
    changedLabels <- results[[1]]$changed_banks
    stats::setNames(lapply(changedLabels, function(b) {
      vals <- lapply(results, function(r) {
        as.numeric(r[[field_full]][b]) - as.numeric(r[[field_comp]][b])
      })
      .bank_aggregate_draws(vals, scenario_diff = TRUE)
    }), changedLabels)
  }

  perBank_delta <- function(field) {
    changedLabels <- results[[1]]$changed_banks
    stats::setNames(lapply(changedLabels, function(b) {
      vals <- lapply(results, function(r) as.numeric(r[[field]][b]))
      .bank_aggregate_draws(vals, scenario_diff = TRUE)
    }), changedLabels)
  }

  counterfactual <- list(
    n_draws = nDraws,
    rates = list(
      pre_L = perBank("r_L_pre"), pre_D = perBank("r_D_pre"),
      comp_L = perBank("r_L_comp"), comp_D = perBank("r_D_comp"),
      full_L = perBank("r_L_full"), full_D = perBank("r_D_full")
    ),
    liquidity = list(
      lambda_comp = perBank("lambda_comp"), lambda_full = perBank("lambda_full"),
      sigma_W_comp = perBank("sigma_W_comp"), sigma_W_full = perBank("sigma_W_full")
    ),
    ## Uncertainty-delta outputs (first-class, per design/ARCHITECTURE.md's
    ## "Counterfactual uncertainty response"): Delta Var(pooled shortfall),
    ## Delta shadow-cost Lambda, Delta E[Gamma] -- for banks whose ownership
    ## changed, comparing the "Full Merger" (dynamic liquidity pooling)
    ## scenario against each bank's OWN frozen pre-merger baseline.
    uncertainty_delta = list(
      delta_var_W = perBank_delta("delta_var_W"),
      delta_lambda = perBank_delta("delta_lambda"),
      delta_E_gamma = perBank_delta("delta_E_gamma")
    ),
    ## Scenario-difference quantities (Full Merger minus Competition-only):
    ## the risk-pooling offset attributable specifically to liquidity, net
    ## of the pure ownership/pricing effect already captured by
    ## Competition-only.
    scenario_diff = list(
      lambda_pooling_offset = perBank_diff("lambda_full", "lambda_comp"),
      rate_L_pooling_offset = perBank_diff("r_L_full", "r_L_comp"),
      rate_D_pooling_offset = perBank_diff("r_D_full", "r_D_comp")
    )
  )

  if (!is.null(ownerPost)) object@model@ownerPost <- .bank_align(ownerPost, labels, "ownerPost")
  object@model@pricePost <- c(results[[1]]$r_L_full, results[[1]]$r_D_full)
  object@diagnostics$route <- "simulate"
  object@diagnostics$counterfactual <- counterfactual
  object
})

## simulate_steps() / respecify(): intentionally NOT overridden for BankFit.
## They inherit FinancialFit's default-reject methods (FinancialArchitecture.R),
## which stop() with an explicit, family-named message. See the roxygen note
## above ("Designed seam for the opt-in primitive-shift path").
