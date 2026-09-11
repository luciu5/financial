#' Bank liquidity-shortfall pricing primitives
#'
#' Migrated (parity migration -- economics preserved exactly, not redesigned)
#' from `stability/code/counterfactual_solver.R` (SHA `cc95e2e0`). These are
#' the closed-form and Monte-Carlo liquidity-shortfall primitives that price
#' the endogenous marginal-cost shifter `Lambda` described in
#' `design/ARCHITECTURE.md`'s Bank module section: liquidity-stress `W-tilde`
#' (a pooled loan-minus-deposit funding gap), a liquid-asset buffer, and a
#' friction parameter `psi` that convexly (quadratically) penalizes shortfall
#' beyond the buffer. There is no regulatory constraint here -- liquidity
#' risk IS the stability mechanism (no capital/LCR/NSFR).
#'
#' Per `design/ARCHITECTURE.md`'s "shared-private/" section, the partial-Beta
#' moments this module needs (`beta_tail_*`, `beta_lower_*`,
#' `beta_inside_mean`/`beta_inside_var`/`beta_inside_shapes`) are the shared,
#' oracle-validated primitives in `shared-beta-moments.R` -- this file does
#' NOT redefine them, it calls them directly. `beta_inside_mean`/
#' `beta_inside_var`/`beta_inside_shapes` are defined ONCE in
#' `shared-beta-moments.R` and used identically by both the debt module (via
#' the h/g focal integrals) and this file.
#'
#' Deliberately NOT migrated (per `design/ARCHITECTURE.md`'s "Do NOT migrate
#' dead code"): `calc_flat_logit_shares`/`stable_softmax`/`bounded_exp`/
#' `calc_flat_logit_markups` (the flat-logit/plogis-outside-share pricing path
#' that the exposure-based solver superseded), `liquidity_shadow_mu`
#' (unused anywhere in `stability`, confirmed via `grep`),
#' `calc_portfolio_liquidity_covariance` (an alternate covariance-based
#' liquidity primitive with no callers in `simulate_merger_exposure.R`/
#' `simulate_merger_market.R`, confirmed via `grep`), `make_ownership`/
#' `merge_owners` (explicitly named as dead code in `design/ARCHITECTURE.md`),
#' and the break-even/systemic-proxy reporting helpers
#' (`make_footprint_distribution_table`, `calc_liquidity_benefit`,
#' `calc_break_even_systemic_cost`, `calc_systemic_proxy_changes`,
#' `make_break_even_row`) -- these are CSV/reporting conveniences for the
#' legacy CLI scripts, not part of the deterministic counterfactual solver's
#' core economics, and are superseded here by the first-class uncertainty-delta
#' outputs described in `design/ARCHITECTURE.md`'s "Counterfactual uncertainty
#' response" (see `BankCounterfactual.R`).
#'
#' @name Bank-risk
#' @keywords internal
#' @include BankCounterfactual.R
NULL

## ---------------------------------------------------------------------
## Normal-approximation excess-second-moment / shadow-price primitives
## (verbatim from counterfactual_solver.R lines 105-115, MINUS the unused
## liquidity_shadow_mu -- see roxygen note above).
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
normal_excess_second_moment <- function(mu_w, sigma_w, buffer) {
  a <- mu_w - buffer
  z <- (buffer - mu_w) / sigma_w
  (sigma_w^2 + a^2) * stats::pnorm(-z) + a * sigma_w * stats::dnorm(z)
}

## ---------------------------------------------------------------------
## Exact single-market Beta excess-moment branch (verbatim from
## counterfactual_solver.R lines 117-235), calling the SHARED beta_tail_*/
## beta_lower_*/beta_inside_* helpers from shared-beta-moments.R instead of
## re-defining them.
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
single_market_beta_excess_moments <- function(exposure_L,
                                               exposure_D,
                                               outside_mu_L,
                                               outside_mu_D,
                                               phi_out_L,
                                               phi_out_D,
                                               liquidity_pressure,
                                               liquid_assets,
                                               psi = 1,
                                               n_quad = 80L) {
  loan_shape <- beta_inside_shapes(outside_mu_L, phi_out_L)
  dep_shape <- beta_inside_shapes(outside_mu_D, phi_out_D)
  q1_L <- beta_inside_mean(outside_mu_L)
  q1_D <- beta_inside_mean(outside_mu_D)
  centered_const <- liquidity_pressure - liquid_assets - exposure_L * q1_L + exposure_D * q1_D

  if (exposure_L <= 1e-12 && exposure_D <= 1e-12) {
    excess <- max(liquidity_pressure - liquid_assets, 0)
    return(list(
      expected_liq_penalty = 0.5 * psi * excess^2,
      second = excess^2,
      tail_probability = as.numeric(excess > 0),
      lambda_score_L = 0,
      lambda_score_D = 0,
      lambda_liq_L = 0,
      lambda_liq_D = 0
    ))
  }

  if (q1_L <= 1e-12 && q1_D <= 1e-12) {
    return(list(
      expected_liq_penalty = 0,
      second = 0,
      tail_probability = 0,
      lambda_score_L = 0,
      lambda_score_D = 0,
      lambda_liq_L = 0,
      lambda_liq_D = 0
    ))
  }

  if (exposure_L > 1e-12 && exposure_D <= 1e-12) {
    base <- centered_const
    cutoff <- -base / exposure_L
    tail0 <- beta_tail_prob(cutoff, loan_shape$shape1, loan_shape$shape2)
    tail1 <- beta_tail_first_moment(cutoff, loan_shape$shape1, loan_shape$shape2)
    tail2 <- beta_tail_second_moment(cutoff, loan_shape$shape1, loan_shape$shape2)
    second <- base^2 * tail0 + 2 * base * exposure_L * tail1 + exposure_L^2 * tail2
    lambda_score_L <- base * (tail1 - q1_L * tail0) +
      exposure_L * (tail2 - q1_L * tail1)
    return(list(
      expected_liq_penalty = 0.5 * psi * max(second, 0),
      second = max(second, 0),
      tail_probability = pmin(pmax(tail0, 0), 1),
      lambda_score_L = lambda_score_L,
      lambda_score_D = 0,
      lambda_liq_L = psi * lambda_score_L / (q1_L + 1e-10),
      lambda_liq_D = 0
    ))
  }

  if (exposure_L <= 1e-12 && exposure_D > 1e-12) {
    base <- centered_const
    cutoff <- base / exposure_D
    lower0 <- beta_lower_prob(cutoff, dep_shape$shape1, dep_shape$shape2)
    lower1 <- beta_lower_first_moment(cutoff, dep_shape$shape1, dep_shape$shape2)
    lower2 <- beta_lower_second_moment(cutoff, dep_shape$shape1, dep_shape$shape2)
    second <- base^2 * lower0 - 2 * base * exposure_D * lower1 + exposure_D^2 * lower2
    lambda_score_D <- q1_D * (base * lower0 - exposure_D * lower1) -
      (base * lower1 - exposure_D * lower2)
    return(list(
      expected_liq_penalty = 0.5 * psi * max(second, 0),
      second = max(second, 0),
      tail_probability = pmin(pmax(lower0, 0), 1),
      lambda_score_L = 0,
      lambda_score_D = lambda_score_D,
      lambda_liq_L = 0,
      lambda_liq_D = psi * lambda_score_D / (q1_D + 1e-10)
    ))
  }

  q_D <- (seq_len(n_quad) - 0.5) / n_quad
  w_D <- stats::dbeta(q_D, dep_shape$shape1, dep_shape$shape2) / n_quad

  second <- 0
  tail_probability <- 0
  lambda_score_L <- 0
  lambda_score_D <- 0
  for (k in seq_along(q_D)) {
    base <- centered_const - exposure_D * q_D[k]
    cutoff <- -base / exposure_L
    tail0 <- beta_tail_prob(cutoff, loan_shape$shape1, loan_shape$shape2)
    tail1 <- beta_tail_first_moment(cutoff, loan_shape$shape1, loan_shape$shape2)
    tail2 <- beta_tail_second_moment(cutoff, loan_shape$shape1, loan_shape$shape2)
    excess_first <- base * tail0 + exposure_L * tail1

    tail_probability <- tail_probability + w_D[k] * tail0
    second <- second + w_D[k] * (
      base^2 * tail0 +
        2 * base * exposure_L * tail1 +
        exposure_L^2 * tail2
    )
    lambda_score_L <- lambda_score_L + w_D[k] * (
      base * (tail1 - q1_L * tail0) +
        exposure_L * (tail2 - q1_L * tail1)
    )
    lambda_score_D <- lambda_score_D + w_D[k] * (q1_D - q_D[k]) * excess_first
  }

  list(
    expected_liq_penalty = 0.5 * psi * max(second, 0),
    second = max(second, 0),
    tail_probability = pmin(pmax(tail_probability, 0), 1),
    lambda_score_L = lambda_score_L,
    lambda_score_D = lambda_score_D,
    lambda_liq_L = psi * lambda_score_L / (q1_L + 1e-10),
    lambda_liq_D = psi * lambda_score_D / (q1_D + 1e-10)
  )
}

## ---------------------------------------------------------------------
## calc_portfolio_liquidity(): the PRIMARY oracle target (parity migration,
## verbatim from counterfactual_solver.R lines 237-336, minus the internal
## `%||%` which now resolves to base R 4.4's built-in null-coalescing
## operator instead of a private definition, per tests/oracles/README.md's
## observation that `%||%` needs no external package on R >= 4.4).
## ---------------------------------------------------------------------

#' Portfolio liquidity-shortfall pricing (closed-form)
#'
#' `calc_portfolio_liquidity()` prices the endogenous marginal-cost shifter
#' `Lambda` -- the shadow cost of liquidity risk -- for a bank's pooled
#' loan/deposit portfolio, given the calibrated Beta participation-share
#' primitives for each market the bank is exposed to. This is the PRIMARY
#' oracle target for the bank module migration (see
#' `design/ARCHITECTURE.md`'s "Oracle strategy": "Bank (strongest):
#' closed-form `calc_portfolio_liquidity` vs Monte-Carlo
#' `simulate_beta_portfolio_liquidity`"). Migrated verbatim from
#' `stability/code/counterfactual_solver.R` (SHA `cc95e2e0`, lines 237-336);
#' see `tests/testthat/test-bank-parity.R` for the executed parity evidence
#' against `tests/oracles/bank_liquidity_mc_vs_cf.rds` and the three limiting-
#' case fixtures.
#'
#' The economics: the bank's liquidity-stress state `W-tilde` is the pooled
#' funding gap (expected loan exposure minus expected deposit exposure minus
#' equity, `mu_W`), a Normal or (for a single market) exact-Beta-mixture
#' random variable. `expected_liq_penalty` is `0.5 * psi *
#' E[max(W-tilde - liquid_assets, 0)^2]` -- the convex quadratic penalty
#' priced into marginal cost. `lambda_liq` (`Lambda`) is the associated
#' marginal shadow cost, split into `lambda_liq_L`/`lambda_liq_D` for the
#' loan- and deposit-side exposure components (these enter `r_L`/`r_D`
#' pricing as an additive markup/markdown wedge; see `BankEquilibrium.R`).
#'
#' @param market_size_L,market_size_D Length-N vectors of loan/deposit
#'   market size for each bank-market row.
#' @param s_cond_L,s_cond_D Length-N vectors of conditional (within-market)
#'   loan/deposit shares.
#' @param outside_mu_L,outside_mu_D Length-N vectors of the calibrated
#'   Beta-participation mean (`mu`) for the outside/non-participation share,
#'   on the loan and deposit side respectively.
#' @param phi_out_L,phi_out_D Length-N vectors of the calibrated Beta
#'   precision (`phi`) for the same participation shares.
#' @param equity,liquid_assets Length-1 bank-level equity and liquid-asset
#'   buffer.
#' @param liquidity_pressure Optional length-1 override for the mean
#'   liquidity-stress state `mu_W`; default `NULL` computes it as
#'   `mean_L - mean_D - equity`.
#' @param psi Length-1 liquidity-friction parameter.
#' @param market_id Length-N vector identifying which market each row
#'   belongs to. Default treats every row as its own market.
#' @param method `"hybrid"` (default) dispatches to the exact single-market
#'   Beta branch (`single_market_beta_excess_moments()`) when there is
#'   exactly one market, and the Normal moment-matching approximation
#'   otherwise. `"normal"` forces the Normal approximation regardless of the
#'   number of markets (used by the primary oracle for an apples-to-apples
#'   comparison against the Monte-Carlo baseline).
#' @param n_quad Quadrature nodes for the exact single-market branch's
#'   deposit-side numerical integration.
#' @return A list; see the source for the full set of fields
#'   (`mu_W`, `sigma_W`, `expected_liq_penalty`, `lambda_liq`,
#'   `lambda_liq_L`, `lambda_liq_D`, `liquidity_method`, ...).
#' @seealso [simulate_beta_portfolio_liquidity()] for the independent
#'   Monte-Carlo comparator used to validate this closed form.
#' @export
calc_portfolio_liquidity <- function(market_size_L,
                                      market_size_D,
                                      s_cond_L,
                                      s_cond_D,
                                      outside_mu_L,
                                      outside_mu_D,
                                      phi_out_L,
                                      phi_out_D,
                                      equity,
                                      liquid_assets,
                                      liquidity_pressure = NULL,
                                      psi,
                                      market_id = seq_along(s_cond_L),
                                      method = c("hybrid", "normal"),
                                      n_quad = 80L) {
  method <- match.arg(method)
  q1_L <- beta_inside_mean(outside_mu_L)
  q1_D <- beta_inside_mean(outside_mu_D)
  v_L <- beta_inside_var(outside_mu_L, phi_out_L)
  v_D <- beta_inside_var(outside_mu_D, phi_out_D)

  exposure_L <- market_size_L * s_cond_L
  exposure_D <- market_size_D * s_cond_D
  mean_L <- sum(exposure_L * q1_L)
  mean_D <- sum(exposure_D * q1_D)
  mu_w <- liquidity_pressure %||% (mean_L - mean_D - equity)

  market_levels <- sort(unique(market_id))
  counts <- as.numeric(table(factor(market_id, levels = market_levels)))
  exposure_market_L <- rowsum(exposure_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1]
  exposure_market_D <- rowsum(exposure_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1]
  outside_mu_L_market <- rowsum(outside_mu_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  outside_mu_D_market <- rowsum(outside_mu_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  phi_out_L_market <- rowsum(phi_out_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  phi_out_D_market <- rowsum(phi_out_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  v_market_L <- rowsum(v_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  v_market_D <- rowsum(v_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1] /
    counts
  var_w_L <- sum(exposure_market_L^2 * v_market_L)
  var_w_D <- sum(exposure_market_D^2 * v_market_D)
  var_w <- var_w_L + var_w_D
  sigma_w <- sqrt(max(var_w, 1e-10))
  liquidity_method <- "multi_market_normal_moment"
  tail_probability <- stats::pnorm((mu_w - liquid_assets) / sigma_w)
  expected_liq_penalty <- 0.5 * psi * normal_excess_second_moment(mu_w, sigma_w, liquid_assets)
  lambda_liq_L <- psi * tail_probability * var_w_L / (mean_L + 1e-10)
  lambda_liq_D <- psi * tail_probability * var_w_D / (mean_D + 1e-10)
  lambda_liq <- 0.5 * (lambda_liq_L + lambda_liq_D)
  lambda_score_L <- NA_real_
  lambda_score_D <- NA_real_

  if (method == "hybrid" && length(market_levels) == 1L) {
    exact_liq <- single_market_beta_excess_moments(
      exposure_L = exposure_market_L[1],
      exposure_D = exposure_market_D[1],
      outside_mu_L = outside_mu_L_market[1],
      outside_mu_D = outside_mu_D_market[1],
      phi_out_L = phi_out_L_market[1],
      phi_out_D = phi_out_D_market[1],
      liquidity_pressure = mu_w,
      liquid_assets = liquid_assets,
      psi = psi,
      n_quad = n_quad
    )
    tail_probability <- exact_liq$tail_probability
    expected_liq_penalty <- exact_liq$expected_liq_penalty
    lambda_liq_L <- exact_liq$lambda_liq_L
    lambda_liq_D <- exact_liq$lambda_liq_D
    lambda_score_L <- exact_liq$lambda_score_L
    lambda_score_D <- exact_liq$lambda_score_D
    lambda_liq <- 0.5 * (lambda_liq_L + lambda_liq_D)
    liquidity_method <- "single_market_beta_exact"
  }

  list(
    mu_W = mu_w,
    sigma_W = sigma_w,
    mean_L = mean_L,
    mean_D = mean_D,
    var_W_L = var_w_L,
    var_W_D = var_w_D,
    liquidity_pressure = mu_w,
    distance_to_distress = (liquid_assets - mu_w) / sigma_w,
    tail_probability = tail_probability,
    expected_liq_penalty = expected_liq_penalty,
    lambda_liq = lambda_liq,
    lambda_liq_L = lambda_liq_L,
    lambda_liq_D = lambda_liq_D,
    lambda_score_L = lambda_score_L,
    lambda_score_D = lambda_score_D,
    liquidity_method = liquidity_method,
    exact_exposure_L = if (length(market_levels) == 1L) exposure_market_L[1] else NA_real_,
    exact_exposure_D = if (length(market_levels) == 1L) exposure_market_D[1] else NA_real_,
    exact_outside_mu_L = if (length(market_levels) == 1L) outside_mu_L_market[1] else NA_real_,
    exact_outside_mu_D = if (length(market_levels) == 1L) outside_mu_D_market[1] else NA_real_,
    exact_phi_out_L = if (length(market_levels) == 1L) phi_out_L_market[1] else NA_real_,
    exact_phi_out_D = if (length(market_levels) == 1L) phi_out_D_market[1] else NA_real_,
    exact_n_quad = n_quad
  )
}

## ---------------------------------------------------------------------
## calibrate_liquidity_pressure_from_wedges(): backs out a bank's pre-merger
## mu_W target from observed rate wedges (parity migration, verbatim from
## counterfactual_solver.R lines 338-422). Used by the bank_fit() calibration
## step (BankCounterfactual.R) exactly as legacy simulate_merger_exposure.R
## uses it in its STEP 3 calibration block.
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
calibrate_liquidity_pressure_from_wedges <- function(liq,
                                                      target_lambda_L,
                                                      target_lambda_D,
                                                      psi,
                                                      liquid_assets) {
  if (!is.null(liq$liquidity_method) && liq$liquidity_method == "single_market_beta_exact") {
    target_values <- numeric(0)
    target_weights <- numeric(0)
    if (is.finite(target_lambda_L) && target_lambda_L > 0) {
      target_values <- c(target_values, target_lambda_L)
      target_weights <- c(target_weights, max(liq$mean_L, 1e-10))
    }
    if (is.finite(target_lambda_D) && target_lambda_D > 0) {
      target_values <- c(target_values, target_lambda_D)
      target_weights <- c(target_weights, max(liq$mean_D, 1e-10))
    }
    if (length(target_values) == 0) {
      return(liquid_assets - 8 * max(liq$sigma_W, 1e-5))
    }

    objective <- function(mu_w) {
      exact_liq <- single_market_beta_excess_moments(
        exposure_L = liq$exact_exposure_L,
        exposure_D = liq$exact_exposure_D,
        outside_mu_L = liq$exact_outside_mu_L,
        outside_mu_D = liq$exact_outside_mu_D,
        phi_out_L = liq$exact_phi_out_L,
        phi_out_D = liq$exact_phi_out_D,
        liquidity_pressure = mu_w,
        liquid_assets = liquid_assets,
        psi = psi,
        n_quad = liq$exact_n_quad %||% 80L
      )
      pred_values <- numeric(0)
      if (is.finite(target_lambda_L) && target_lambda_L > 0) {
        pred_values <- c(pred_values, exact_liq$lambda_liq_L)
      }
      if (is.finite(target_lambda_D) && target_lambda_D > 0) {
        pred_values <- c(pred_values, exact_liq$lambda_liq_D)
      }
      stats::weighted.mean(pred_values - target_values, target_weights)
    }

    span <- max(10 * max(liq$sigma_W, 1e-5), max(abs(liq$mean_L), abs(liq$mean_D), liquid_assets, 1))
    lower <- liquid_assets - span
    upper <- liquid_assets + span
    f_lower <- objective(lower)
    f_upper <- objective(upper)
    expand_iter <- 0
    while (is.finite(f_lower) && is.finite(f_upper) && f_lower * f_upper > 0 && expand_iter < 8) {
      span <- span * 2
      lower <- liquid_assets - span
      upper <- liquid_assets + span
      f_lower <- objective(lower)
      f_upper <- objective(upper)
      expand_iter <- expand_iter + 1
    }
    if (is.finite(f_lower) && is.finite(f_upper) && f_lower * f_upper <= 0) {
      return(stats::uniroot(objective, lower = lower, upper = upper)$root)
    }
    return(if (abs(f_lower) < abs(f_upper)) lower else upper)
  }

  scale_L <- psi * liq$var_W_L / (liq$mean_L + 1e-10)
  scale_D <- psi * liq$var_W_D / (liq$mean_D + 1e-10)

  tail_targets <- numeric(0)
  tail_weights <- numeric(0)
  if (is.finite(target_lambda_L) && target_lambda_L > 0 && is.finite(scale_L) && scale_L > 0) {
    tail_targets <- c(tail_targets, target_lambda_L / scale_L)
    tail_weights <- c(tail_weights, max(liq$mean_L, 1e-10))
  }
  if (is.finite(target_lambda_D) && target_lambda_D > 0 && is.finite(scale_D) && scale_D > 0) {
    tail_targets <- c(tail_targets, target_lambda_D / scale_D)
    tail_weights <- c(tail_weights, max(liq$mean_D, 1e-10))
  }

  if (length(tail_targets) == 0) {
    target_tail <- 1e-8
  } else {
    target_tail <- stats::weighted.mean(pmin(pmax(tail_targets, 1e-8), 1 - 1e-8), tail_weights)
  }

  liquid_assets + liq$sigma_W * stats::qnorm(pmin(pmax(target_tail, 1e-8), 1 - 1e-8))
}

## ---------------------------------------------------------------------
## simulate_beta_portfolio_liquidity(): independent Monte-Carlo comparator
## used ONLY by the parity test (matches tests/oracles/bank_liquidity_mc_vs_cf.rds).
## Not part of the deterministic solver's hot path, but migrated verbatim
## (counterfactual_solver.R lines 424-473) since the oracle fixture directly
## checks it and design/ARCHITECTURE.md names it as the oracle-strategy
## Monte-Carlo comparator.
## ---------------------------------------------------------------------

#' Monte-Carlo portfolio liquidity-shortfall pricing (independent comparator)
#'
#' Independent Monte-Carlo implementation of the same liquidity-shortfall
#' pricing problem as [calc_portfolio_liquidity()], used ONLY to validate the
#' closed form (see `tests/testthat/test-bank-parity.R` and
#' `design/ARCHITECTURE.md`'s "Oracle strategy"). Migrated verbatim from
#' `stability/code/counterfactual_solver.R` (SHA `cc95e2e0`, lines 424-473).
#'
#' @inheritParams calc_portfolio_liquidity
#' @param draws Number of Monte-Carlo draws.
#' @param seed Optional RNG seed.
#' @return A list with `expected_liq_penalty`, `lambda_liq`, `n_markets`.
#' @export
simulate_beta_portfolio_liquidity <- function(market_size_L,
                                               market_size_D,
                                               s_cond_L,
                                               s_cond_D,
                                               outside_mu_L,
                                               outside_mu_D,
                                               phi_out_L,
                                               phi_out_D,
                                               equity,
                                               liquid_assets,
                                               liquidity_pressure = NULL,
                                               psi,
                                               market_id = seq_along(s_cond_L),
                                               draws = 50000L,
                                               seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  exposure_L <- market_size_L * s_cond_L
  exposure_D <- market_size_D * s_cond_D
  market_levels <- sort(unique(market_id))
  exposure_market_L <- rowsum(exposure_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1]
  exposure_market_D <- rowsum(exposure_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1]
  counts <- as.numeric(table(factor(market_id, levels = market_levels)))
  mu_L <- rowsum(outside_mu_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  mu_D <- rowsum(outside_mu_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  phi_L <- rowsum(phi_out_L, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  phi_D <- rowsum(phi_out_D, group = market_id, reorder = FALSE)[as.character(market_levels), 1] / counts
  shape_L <- beta_inside_shapes(mu_L, phi_L)
  shape_D <- beta_inside_shapes(mu_D, phi_D)
  q1_L <- beta_inside_mean(mu_L)
  q1_D <- beta_inside_mean(mu_D)
  mu_w <- liquidity_pressure %||% (
    sum(exposure_market_L * q1_L) - sum(exposure_market_D * q1_D) - equity
  )

  q_L <- matrix(stats::rbeta(draws * length(market_levels), shape_L$shape1, shape_L$shape2),
    nrow = draws, byrow = TRUE
  )
  q_D <- matrix(stats::rbeta(draws * length(market_levels), shape_D$shape1, shape_D$shape2),
    nrow = draws, byrow = TRUE
  )
  w <- mu_w +
    as.numeric((q_L - matrix(q1_L, nrow = draws, ncol = length(q1_L), byrow = TRUE)) %*% exposure_market_L) +
    as.numeric((matrix(q1_D, nrow = draws, ncol = length(q1_D), byrow = TRUE) - q_D) %*% exposure_market_D)
  excess <- pmax(w - liquid_assets, 0)

  list(
    expected_liq_penalty = 0.5 * psi * mean(excess^2),
    lambda_liq = psi * mean(excess),
    n_markets = length(market_levels)
  )
}
