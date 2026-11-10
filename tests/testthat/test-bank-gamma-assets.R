# A permanent: a correct asset-weighted equilibrium can still report a wrong
# Delta E[Gamma] if diagnostics independently substitute liquid assets + equity.
test_that("reported liquidity-penalty change uses total-asset weights at solved rates", {
  data <- .make_bank_test_data()
  data$assets <- c(A = 500, B = 50, C = 5)
  data$r_L <- rep(.08, 6)
  data$r_D <- rep(.005, 6)
  # Invert the smooth capped log link so calibrated psi is nonconstant without
  # mutating a fitted baseline or its calibrated liquidity-pressure state.
  data$Z_psi <- matrix(20 * atanh(log(c(.20, 2, 1)) / 20), 3, 1)
  parameters <- .make_bank_test_params()
  parameters["gamma_psi"] <- 1
  parameters[c("alpha_bar_L", "alpha_bar_D")] <- log(100)
  fit <- bank_fit(parameters, data)
  result <- simulate(fit, ownerPost = c(A = "AB", B = "AB", C = "C"))
  pre <- fit@model
  post <- result@model
  members <- match(c("A", "B"), post@labels)
  rows <- which(post@bankRow %in% post@labels[members])
  shares <- calcShares(post, preMerger = FALSE)

  # Independent two-market Normal shortfall integral: aggregate each market's
  # exposures before squaring, then apply the quadratic positive-part identity.
  # No pooling/weighting or portfolio-penalty helper is used for this comparator.
  penalty <- function(model, rows, s_L, s_D, mu_L, mu_D, psi, pressure, buffer) {
    markets <- model@marketID[rows]
    exposure_L <- model@marketSize_L[rows] * s_L
    exposure_D <- model@marketSize_D[rows] * s_D
    v_L <- mu_L * (1 - mu_L) / (model@phiOut_L[rows] + 1)
    v_D <- mu_D * (1 - mu_D) / (model@phiOut_D[rows] + 1)
    variance <- sum(tapply(exposure_L, markets, sum)^2 * tapply(v_L, markets, mean)) +
      sum(tapply(exposure_D, markets, sum)^2 * tapply(v_D, markets, mean))
    sigma <- sqrt(max(variance, 1e-10))
    gap <- pressure - buffer
    z <- gap / sigma
    psi * ((gap^2 + sigma^2) * stats::pnorm(z) + gap * sigma * stats::dnorm(z)) / 2
  }
  expected_L <- sum(post@marketSize_L[rows] * shares$s_cond_L[rows] * shares$q1_L[rows])
  expected_D <- sum(post@marketSize_D[rows] * shares$s_cond_D[rows] * shares$q1_D[rows])
  pressure <- sum(pre@liquidityPressure[members]) + expected_L - sum(pre@baseMean_L[members]) +
    sum(pre@baseMean_D[members]) - expected_D
  psi_assets <- sum(pre@psi[members] * pre@assets[members]) / sum(pre@assets[members])
  proxy_weights <- pre@liquidAssets[members] + pre@equity[members]
  psi_proxy <- sum(pre@psi[members] * proxy_weights) / sum(proxy_weights)
  expected_post <- penalty(post, rows, shares$s_cond_L[rows], shares$s_cond_D[rows],
    shares$outside_mu_L[rows], shares$outside_mu_D[rows], psi_assets,
    pressure, sum(pre@liquidAssets[members]))
  wrong_post <- expected_post * psi_proxy / psi_assets
  expect_gt(abs(wrong_post - expected_post), 1e-3)
  for (b in members) {
    baseline_rows <- which(pre@bankRow == pre@labels[b])
    expected_pre <- penalty(pre, baseline_rows, pre@sCondBase_L[baseline_rows],
      pre@sCondBase_D[baseline_rows], pre@outsideMu_L[baseline_rows],
      pre@outsideMu_D[baseline_rows], pre@psi[b], pre@liquidityPressure[b], pre@liquidAssets[b])
    reported <- result@diagnostics$counterfactual$uncertainty_delta$delta_E_gamma[[pre@labels[b]]]
    expect_equal(reported$value, unname(expected_post - expected_pre), tolerance = 1e-8)
    expect_identical(reported$n_draws, 1L)
  }
})
