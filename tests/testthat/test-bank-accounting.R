test_that("portfolio liquidity matches independent funding-gap and shortfall formulas", {
  # Two independent markets, with a loan and deposit side in each.
  size_L <- c(100, 80); size_D <- c(90, 70)
  share_L <- c(.20, .30); share_D <- c(.25, .20)
  outside_L <- c(.25, .40); outside_D <- c(.50, .30)
  phi_L <- c(4, 9); phi_D <- c(6, 8)
  equity <- 2; buffer <- 5; psi <- 1.5

  got <- calc_portfolio_liquidity(
    market_size_L = size_L, market_size_D = size_D,
    s_cond_L = share_L, s_cond_D = share_D,
    outside_mu_L = outside_L, outside_mu_D = outside_D,
    phi_out_L = phi_L, phi_out_D = phi_D,
    equity = equity, liquid_assets = buffer, psi = psi,
    market_id = 1:2, method = "normal"
  )

  exposure_L <- size_L * share_L
  exposure_D <- size_D * share_D
  mean_L <- sum(exposure_L * (1 - outside_L))
  mean_D <- sum(exposure_D * (1 - outside_D))
  # The variance of a Beta(mu, phi) participation share is mu(1-mu)/(phi+1).
  var_L <- sum(exposure_L^2 * outside_L * (1 - outside_L) / (phi_L + 1))
  var_D <- sum(exposure_D^2 * outside_D * (1 - outside_D) / (phi_D + 1))
  mu_W <- mean_L - mean_D - equity
  sigma_W <- sqrt(var_L + var_D)
  gap <- mu_W - buffer
  z <- gap / sigma_W
  expected_shortfall_sq <- (gap^2 + sigma_W^2) * stats::pnorm(z) +
    gap * sigma_W * stats::dnorm(z)

  expect_equal(got$mean_L, mean_L, tolerance = 1e-12)
  expect_equal(got$mean_D, mean_D, tolerance = 1e-12)
  expect_equal(got$mu_W, mu_W, tolerance = 1e-12)
  expect_equal(got$var_W_L, var_L, tolerance = 1e-12)
  expect_equal(got$var_W_D, var_D, tolerance = 1e-12)
  expect_equal(got$sigma_W, sigma_W, tolerance = 1e-12)
  expect_equal(got$expected_liq_penalty, psi * expected_shortfall_sq / 2,
    tolerance = 1e-10)
  numeric_penalty <- stats::integrate(
    function(w) psi * (w - buffer)^2 *
      stats::dnorm(w, mean = mu_W, sd = sigma_W) / 2,
    lower = buffer, upper = Inf
  )$value
  expect_equal(got$expected_liq_penalty, numeric_penalty, tolerance = 1e-8)
})
