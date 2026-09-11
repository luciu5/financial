## Parity regression tests for the migrated bank module.
##
## Rule of use (tests/oracles/README.md): the migrated `financial` code must
## be tested AGAINST THE PRE-MIGRATION FIXTURES captured from legacy
## stability (SHA cc95e2e0) execution, never against itself.

mc_vs_cf_path <- testthat::test_path("..", "oracles", "bank_liquidity_mc_vs_cf.rds")
phi_large_path <- testthat::test_path("..", "oracles", "bank_limiting_phi_large.rds")
psi_zero_path <- testthat::test_path("..", "oracles", "bank_limiting_psi_zero.rds")
single_multi_path <- testthat::test_path("..", "oracles", "bank_limiting_single_vs_multi_market.rds")

## ---------------------------------------------------------------------
## 1. PRIMARY oracle: closed-form vs Monte-Carlo, multi-market portfolio.
## ---------------------------------------------------------------------

test_that("calc_portfolio_liquidity() reproduces the closed-form oracle EXACTLY", {
  skip_if_not(file.exists(mc_vs_cf_path), "bank_liquidity_mc_vs_cf.rds oracle not present")
  oracle <- readRDS(mc_vs_cf_path)
  inputs <- oracle$inputs

  cf <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = inputs$market_id, method = "normal"
  )

  ## Deterministic, same inputs -> tight tolerance (~1e-8), per the migration brief.
  expect_equal(cf$expected_liq_penalty, oracle$closed_form$expected_liq_penalty, tolerance = 1e-8)
  expect_equal(cf$mu_W, oracle$closed_form$mu_W, tolerance = 1e-8)
  expect_equal(cf$sigma_W, oracle$closed_form$sigma_W, tolerance = 1e-8)
  expect_equal(cf$lambda_liq, oracle$closed_form$lambda_liq, tolerance = 1e-8)
  expect_equal(cf$lambda_liq_L, oracle$closed_form$lambda_liq_L, tolerance = 1e-8)
  expect_equal(cf$lambda_liq_D, oracle$closed_form$lambda_liq_D, tolerance = 1e-8)
  expect_identical(cf$liquidity_method, oracle$closed_form$liquidity_method)
})

test_that("closed-form expected_liq_penalty agrees with the stored Monte-Carlo value within documented MC noise", {
  skip_if_not(file.exists(mc_vs_cf_path), "bank_liquidity_mc_vs_cf.rds oracle not present")
  oracle <- readRDS(mc_vs_cf_path)
  inputs <- oracle$inputs

  cf <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = inputs$market_id, method = "normal"
  )

  ## README documents the captured MC/CF relative gap as 2.31% (ordinary MC
  ## noise at draws=200000, characterized by repeat-seed runs in [1.0%,2.0%]).
  ## Compare our closed form against the STORED MC value (not re-running MC,
  ## since we assert against the pre-migration fixture, not re-simulate).
  rel_diff <- abs(cf$expected_liq_penalty - oracle$monte_carlo$expected_liq_penalty) / cf$expected_liq_penalty
  expect_lt(rel_diff, 0.05) # documented band: observed 2.31%, allow up to 5%

  ## Also directly reproduce our own closed form's stored value at 1e-8.
  expect_equal(cf$expected_liq_penalty, oracle$comparison$cf_expected_liq_penalty, tolerance = 1e-8)
})

test_that("simulate_beta_portfolio_liquidity() Monte-Carlo comparator is migrated and runs", {
  skip_if_not(file.exists(mc_vs_cf_path), "bank_liquidity_mc_vs_cf.rds oracle not present")
  oracle <- readRDS(mc_vs_cf_path)
  inputs <- oracle$inputs

  mc <- simulate_beta_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = inputs$market_id, draws = 200000L, seed = inputs$seed
  )

  ## Re-running the SAME seed/draws as the capture script should reproduce
  ## the stored Monte-Carlo value exactly (same RNG stream).
  expect_equal(mc$expected_liq_penalty, oracle$monte_carlo$expected_liq_penalty, tolerance = 1e-8)
  expect_equal(mc$lambda_liq, oracle$monte_carlo$lambda_liq, tolerance = 1e-8)
})

## ---------------------------------------------------------------------
## 2. Limiting cases.
## ---------------------------------------------------------------------

test_that("phi -> large gives expected_liq_penalty == 0 (matches bank_limiting_phi_large.rds)", {
  skip_if_not(file.exists(phi_large_path), "bank_limiting_phi_large.rds oracle not present")
  oracle <- readRDS(phi_large_path)
  inputs <- oracle$inputs
  n_markets <- length(inputs$market_size_L)

  result <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = rep(inputs$phi, n_markets), phi_out_D = rep(inputs$phi, n_markets),
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = inputs$market_id, method = "normal"
  )

  expect_equal(result$expected_liq_penalty, 0, tolerance = 1e-8)
  expect_equal(result$expected_liq_penalty, oracle$result$expected_liq_penalty, tolerance = 1e-8)
  expect_equal(result$mu_W, oracle$result$mu_W, tolerance = 1e-8)
  expect_equal(result$sigma_W, oracle$result$sigma_W, tolerance = 1e-8)

  deterministic_shortfall <- 0.5 * inputs$psi * max(result$mu_W - inputs$liquid_assets, 0)^2
  expect_equal(deterministic_shortfall, oracle$deterministic_shortfall_formula, tolerance = 1e-8)
  expect_equal(abs(result$expected_liq_penalty - deterministic_shortfall), oracle$abs_diff, tolerance = 1e-8)
})

test_that("psi = 0 gives expected_liq_penalty == 0 exactly (matches bank_limiting_psi_zero.rds)", {
  skip_if_not(file.exists(psi_zero_path), "bank_limiting_psi_zero.rds oracle not present")
  oracle <- readRDS(psi_zero_path)
  inputs <- oracle$inputs

  result <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = inputs$market_id, method = "normal"
  )

  expect_equal(result$expected_liq_penalty, 0, tolerance = 1e-12)
  expect_equal(result$lambda_liq, 0, tolerance = 1e-12)
  expect_equal(result$expected_liq_penalty, oracle$result$expected_liq_penalty, tolerance = 1e-8)
  expect_equal(result$lambda_liq, oracle$result$lambda_liq, tolerance = 1e-8)
})

test_that("single-vs-multi-market values match bank_limiting_single_vs_multi_market.rds", {
  skip_if_not(file.exists(single_multi_path), "bank_limiting_single_vs_multi_market.rds oracle not present")
  oracle <- readRDS(single_multi_path)
  inputs <- oracle$inputs
  n_markets <- length(inputs$market_size_L)
  single_market_id <- rep(1, n_markets)
  multi_market_id <- seq_len(n_markets)

  single_hybrid <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = single_market_id, method = "hybrid"
  )
  single_normal <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = single_market_id, method = "normal"
  )
  multi_normal <- calc_portfolio_liquidity(
    market_size_L = inputs$market_size_L, market_size_D = inputs$market_size_D,
    s_cond_L = inputs$s_cond_L, s_cond_D = inputs$s_cond_D,
    outside_mu_L = inputs$outside_mu_L, outside_mu_D = inputs$outside_mu_D,
    phi_out_L = inputs$phi_out_L, phi_out_D = inputs$phi_out_D,
    equity = inputs$equity, liquid_assets = inputs$liquid_assets, psi = inputs$psi,
    market_id = multi_market_id, method = "normal"
  )

  expect_identical(single_hybrid$liquidity_method, "single_market_beta_exact")
  expect_equal(single_hybrid$expected_liq_penalty, oracle$single_market_exact_hybrid$expected_liq_penalty, tolerance = 1e-8)

  expect_identical(single_normal$liquidity_method, "multi_market_normal_moment")
  expect_equal(single_normal$expected_liq_penalty, oracle$single_market_normal_forced$expected_liq_penalty, tolerance = 1e-8)

  expect_equal(multi_normal$expected_liq_penalty, oracle$multi_market_normal$expected_liq_penalty, tolerance = 1e-8)

  ## Diversification is a real economic feature: pooling 5 quasi-independent
  ## markets into 1 must show LOWER liquidity cost than treating them as
  ## genuinely separate markets is the wrong direction to test (multi-market
  ## has SMALLER variance than the single-market collapse, since collapsing
  ## removes diversification) -- assert the documented ordering:
  ## multi_market_normal << single_market_normal_forced, and
  ## single_market_exact_hybrid << single_market_normal_forced (Beta-vs-Normal
  ## approximation error on the same undiversified inputs).
  expect_lt(multi_normal$expected_liq_penalty, single_normal$expected_liq_penalty)
  expect_lt(single_hybrid$expected_liq_penalty, single_normal$expected_liq_penalty)
})

## ---------------------------------------------------------------------
## 3. bank_fit() round-trip: single-draw posterior == point-estimate path.
## ---------------------------------------------------------------------

#' Build a small synthetic bank_fit() `data` list for round-trip testing.
#' 2 banks, 2 markets each (one shared/overlap market so a merger has a
#' non-trivial FOC interaction), fabricated but internally consistent.
.make_bank_test_data <- function() {
  bank_id <- c("A", "A", "B", "B", "C", "C")
  market_id <- c("m1", "m2", "m1", "m2", "m1", "m2")
  n <- length(bank_id)

  list(
    bank_id = bank_id, market_id = market_id,
    s_cond_L = c(0.4, 0.3, 0.35, 0.4, 0.25, 0.3),
    s_cond_D = c(0.3, 0.35, 0.4, 0.3, 0.3, 0.35),
    market_size_L = rep(100, n), market_size_D = rep(120, n),
    r_L = rep(0.05, n), r_D = rep(0.02, n), r_f = rep(0.03, n),
    W_L = matrix(0, n, 0), W_D = matrix(0, n, 0),
    H_out_L = matrix(1, n, 1), H_out_D = matrix(1, n, 1),
    out_iv_L = rep(0, n), out_iv_D = rep(0, n),
    market_state_region_id = rep(1L, n),
    group_id = bank_id,
    Z_psi = matrix(c(0, 0, 0), nrow = 3, ncol = 1),
    equity = c(A = 10, B = 8, C = 12),
    liquid_assets = c(A = 5, B = 4, C = 6)
  )
}

.make_bank_test_params <- function() {
  c(
    alpha_bar_L = log(3), alpha_bar_D = log(3),
    sigma_alpha_state_L = 0, sigma_alpha_state_D = 0,
    `alpha_state_raw_L[1]` = 0, `alpha_state_raw_D[1]` = 0,
    `theta_out_L[1]` = 0, `theta_out_D[1]` = 0,
    alpha_out_L = -1.7, alpha_out_D = -1.7,
    log_phi_bar_L = log(15), log_phi_bar_D = log(15),
    gamma_psi = 0
  )
}

test_that("bank_fit() single-draw vector reproduces the point-estimate path", {
  data <- .make_bank_test_data()
  params <- .make_bank_test_params()

  fit_vec <- bank_fit(parameters = params, data = data)
  expect_s4_class(fit_vec, "BankFit")
  expect_s4_class(fit_vec@model, "BankMarket")
  expect_equal(fit_vec@diagnostics$n_draws, 1L)

  ## Wrapping the SAME named vector as a 1-row matrix (the "single-draw
  ## posterior" shape) must give an IDENTICAL BankMarket -- point estimate
  ## and single draw are the SAME code path (design/STAN-BANKFIT-MAPPING.md).
  params_mat <- matrix(params, nrow = 1, dimnames = list(NULL, names(params)))
  fit_mat <- bank_fit(parameters = params_mat, data = data)
  expect_equal(fit_mat@diagnostics$n_draws, 1L)

  expect_equal(fit_vec@model@pricePre, fit_mat@model@pricePre, tolerance = 1e-10)
  expect_equal(fit_vec@model@alpha_L, fit_mat@model@alpha_L, tolerance = 1e-10)
  expect_equal(fit_vec@model@baseLambda_L, fit_mat@model@baseLambda_L, tolerance = 1e-10)
  expect_equal(fit_vec@model@omega_L, fit_mat@model@omega_L, tolerance = 1e-10)

  ## Also accept the "name.i" dot convention for vector parameters.
  params_dot <- params
  names(params_dot) <- gsub("\\[(\\d+)\\]$", ".\\1", names(params_dot))
  fit_dot <- bank_fit(parameters = params_dot, data = data)
  expect_equal(fit_vec@model@alpha_L, fit_dot@model@alpha_L, tolerance = 1e-10)
})

test_that("bank_fit()/simulate() runs the merger counterfactual and produces uncertainty-delta outputs", {
  data <- .make_bank_test_data()
  params <- .make_bank_test_params()
  fit <- bank_fit(parameters = params, data = data)

  ownerPost <- c(A = "A", B = "A", C = "C") # A acquires B
  result <- simulate(fit, ownerPost = ownerPost)

  expect_s4_class(result, "BankFit")
  cf <- result@diagnostics$counterfactual
  expect_true(all(c("rates", "liquidity", "uncertainty_delta", "scenario_diff") %in% names(cf)))

  ## Uncertainty-delta outputs are first-class and computed for the merging
  ## banks (A, B) -- Delta Var(pooled shortfall), Delta shadow-cost Lambda,
  ## Delta E[Gamma] (design/ARCHITECTURE.md's "Counterfactual uncertainty
  ## response").
  expect_setequal(names(cf$uncertainty_delta$delta_var_W), c("A", "B"))
  expect_setequal(names(cf$uncertainty_delta$delta_lambda), c("A", "B"))
  expect_setequal(names(cf$uncertainty_delta$delta_E_gamma), c("A", "B"))
  for (nm in c("A", "B")) {
    expect_true(is.finite(cf$uncertainty_delta$delta_var_W[[nm]]$value))
    expect_true(is.finite(cf$uncertainty_delta$delta_lambda[[nm]]$value))
    expect_true(is.finite(cf$uncertainty_delta$delta_E_gamma[[nm]]$value))
  }

  ## Bank C (uninvolved in the merger) is untouched by the counterfactual.
  expect_false("C" %in% names(cf$uncertainty_delta$delta_var_W))
})

test_that("bank_fit()/simulate() maps over a multi-draw posterior and aggregates per the recipe", {
  data <- .make_bank_test_data()
  base_params <- .make_bank_test_params()

  set.seed(1)
  draws_mat <- do.call(rbind, lapply(1:5, function(i) {
    p <- base_params
    p["log_phi_bar_L"] <- p["log_phi_bar_L"] + stats::rnorm(1, 0, 0.05)
    p["log_phi_bar_D"] <- p["log_phi_bar_D"] + stats::rnorm(1, 0, 0.05)
    p
  }))

  fit <- bank_fit(parameters = draws_mat, data = data)
  expect_equal(fit@diagnostics$n_draws, 5L)

  ownerPost <- c(A = "A", B = "A", C = "C")
  result <- simulate(fit, ownerPost = ownerPost)
  cf <- result@diagnostics$counterfactual

  ## For >1 draw, aggregated fields carry mean/sd/q05/q50/q95 + n_draws.
  agg <- cf$uncertainty_delta$delta_lambda[["A"]]
  expect_true(all(c("mean", "sd", "q05", "q50", "q95", "n_draws") %in% names(agg)))
  expect_equal(agg$n_draws, 5L)

  ## Scenario-difference quantities additionally carry posterior_prob_positive
  ## / posterior_odds_positive per design/STAN-BANKFIT-MAPPING.md's
  ## aggregation recipe.
  diff_agg <- cf$scenario_diff$lambda_pooling_offset[["A"]]
  expect_true(all(c("posterior_prob_positive", "posterior_odds_positive") %in% names(diff_agg)))
  expect_true(diff_agg$posterior_prob_positive >= 0 && diff_agg$posterior_prob_positive <= 1)
})
