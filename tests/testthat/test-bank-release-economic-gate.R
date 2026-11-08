## Migration parity A (permanent).
## This independent economic gate catches a material bank-merger failure
## missed by the existing oracle and round-trip tests: ownership aggregation or
## the competition-only/full-liquidity decomposition can return rates that do
## not satisfy the model's bank-level Nash-in-rates FOCs on a multi-market,
## three-bank merged footprint with an independent competitor.

.release_gate_softmax <- function(model, r_L, r_D) {
  row_bank <- match(model@bankRow, model@labels)
  r_L_row <- r_L[row_bank]
  r_D_row <- r_D[row_bank]
  ref <- ave(seq_along(model@marketID), model@marketID, FUN = function(i) i[1])

  conditional <- function(base_share, base_rate, rate, alpha) {
    utility_at_reference <- log(base_share) - log(base_share[ref]) -
      alpha * (base_rate - base_rate[ref])
    utility <- utility_at_reference + alpha * (rate - rate[ref])
    raw <- exp(utility)
    raw / ave(raw, model@marketID, FUN = sum)
  }

  s_L <- conditional(model@sCondBase_L, model@rBase_L, r_L_row, model@alpha_L)
  s_D <- conditional(model@sCondBase_D, model@rBase_D, r_D_row, model@alpha_D)

  participation <- function(outside_mu, phi, base_share, alpha, rate, base_rate) {
    q1_base <- 1 - outside_mu
    odds_base <- q1_base / outside_mu
    market_scale <- ave(base_share * exp(alpha * (rate - base_rate)),
      model@marketID, FUN = sum
    )
    odds <- odds_base * exp(pmax(pmin(log(market_scale), 20), -20))
    q1 <- odds / (1 + odds)
    q2 <- q1 * (phi * q1 + 1) / (phi + 1)
    list(q1 = q1, q2 = q2, outside_mu = 1 - q1)
  }

  p_L <- participation(model@outsideMu_L, model@phiOut_L,
    model@sCondBase_L, model@alpha_L, r_L_row, model@rBase_L
  )
  p_D <- participation(model@outsideMu_D, model@phiOut_D,
    model@sCondBase_D, model@alpha_D, r_D_row, model@rBase_D
  )

  list(s_L = s_L, s_D = s_D, p_L = p_L, p_D = p_D, row_bank = row_bank)
}

.release_gate_quantities_and_derivatives <- function(model, shares) {
  J <- length(model@labels)
  row_bank <- shares$row_bank
  q_L <- model@marketSize_L * shares$s_L * shares$p_L$q1
  q_D <- model@marketSize_D * shares$s_D * shares$p_D$q1
  Q_L <- vapply(seq_len(J), function(j) sum(q_L[row_bank == j]), numeric(1))
  Q_D <- vapply(seq_len(J), function(j) sum(q_D[row_bank == j]), numeric(1))
  D_L <- matrix(0, J, J)
  D_D <- matrix(0, J, J)

  for (market in unique(model@marketID)) {
    rows <- which(model@marketID == market)
    for (a in rows) {
      i <- row_bank[a]
      own_L <- model@alpha_L[a] * model@marketSize_L[a] *
        (shares$s_L[a] * shares$p_L$q1[a] - shares$s_L[a]^2 * shares$p_L$q2[a])
      own_D <- model@alpha_D[a] * model@marketSize_D[a] *
        (shares$s_D[a] * shares$p_D$q1[a] - shares$s_D[a]^2 * shares$p_D$q2[a])
      D_L[i, i] <- D_L[i, i] + own_L
      D_D[i, i] <- D_D[i, i] + own_D
      for (b in setdiff(rows, a)) {
        j <- row_bank[b]
        D_L[i, j] <- D_L[i, j] - model@alpha_L[a] * model@marketSize_L[a] *
          shares$s_L[a] * shares$s_L[b] * shares$p_L$q2[a]
        D_D[i, j] <- D_D[i, j] - model@alpha_D[a] * model@marketSize_D[a] *
          shares$s_D[a] * shares$s_D[b] * shares$p_D$q2[a]
      }
    }
  }

  own <- outer(model@ownerPost, model@ownerPost, FUN = "==") * 1
  F_L <- own * t(D_L)
  F_D <- own * t(D_D)
  list(
    Q_L = Q_L, Q_D = Q_D, D_L = D_L, D_D = D_D,
    F_L = F_L, F_D = F_D,
    markup_L = -as.numeric(solve(F_L, Q_L)),
    markdown_D = as.numeric(solve(F_D, Q_D)),
    q_L = q_L, q_D = q_D
  )
}

.release_gate_pooled_liquidity <- function(model, shares) {
  n <- length(model@labels)
  lambda_L <- model@baseLambda_L
  lambda_D <- model@baseLambda_D
  changed_owner <- unique(model@ownerPost[model@ownerPost != model@ownerPre])
  if (!length(changed_owner)) return(list(lambda_L = lambda_L, lambda_D = lambda_D))

  row_bank <- shares$row_bank
  q1_base_L <- 1 - model@outsideMu_L
  q1_base_D <- 1 - model@outsideMu_D
  base_mean_L <- vapply(seq_len(n), function(j) {
    rows <- which(row_bank == j)
    sum(model@marketSize_L[rows] * model@sCondBase_L[rows] * q1_base_L[rows])
  }, numeric(1))
  base_mean_D <- vapply(seq_len(n), function(j) {
    rows <- which(row_bank == j)
    sum(model@marketSize_D[rows] * model@sCondBase_D[rows] * q1_base_D[rows])
  }, numeric(1))

  for (owner in changed_owner) {
    banks <- which(model@ownerPost == owner)
    rows <- which(row_bank %in% banks)
    expected_L <- sum(model@marketSize_L[rows] * shares$s_L[rows] * shares$p_L$q1[rows])
    expected_D <- sum(model@marketSize_D[rows] * shares$s_D[rows] * shares$p_D$q1[rows])
    pressure <- sum(model@liquidityPressure[banks]) +
      expected_L - sum(base_mean_L[banks]) + sum(base_mean_D[banks]) - expected_D

    psi <- if (length(banks) == 1L) {
      model@psi[banks]
    } else if (model@psiRule == "asset_weighted") {
      stats::weighted.mean(model@psi[banks], w = pmax(model@assets[banks], 1))
    } else if (model@psiRule == "acquirer") {
      model@psi[banks[1]]
    } else if (model@psiRule == "target") {
      model@psi[banks[length(banks)]]
    } else {
      min(model@psi[banks])
    }

    exp_L <- model@marketSize_L[rows] * shares$s_L[rows]
    exp_D <- model@marketSize_D[rows] * shares$s_D[rows]
    mean_L <- sum(exp_L * shares$p_L$q1[rows])
    mean_D <- sum(exp_D * shares$p_D$q1[rows])
    var_L <- var_D <- 0
    for (market in unique(model@marketID[rows])) {
      in_market <- rows[model@marketID[rows] == market]
      # One participation draw is shared by products in a market, so its
      # aggregate exposure, rather than each row's exposure, is squared.
      eL <- sum(model@marketSize_L[in_market] * shares$s_L[in_market])
      eD <- sum(model@marketSize_D[in_market] * shares$s_D[in_market])
      vL <- mean(shares$p_L$q2[in_market] - shares$p_L$q1[in_market]^2)
      vD <- mean(shares$p_D$q2[in_market] - shares$p_D$q1[in_market]^2)
      var_L <- var_L + eL^2 * vL
      var_D <- var_D + eD^2 * vD
    }

    sigma <- sqrt(max(var_L + var_D, 1e-10))
    tail <- stats::pnorm((pressure - sum(model@liquidAssets[banks])) / sigma)
    lambda_L[banks] <- psi * tail * var_L / (mean_L + 1e-10)
    lambda_D[banks] <- psi * tail * var_D / (mean_D + 1e-10)
  }
  list(lambda_L = lambda_L, lambda_D = lambda_D)
}

.release_gate_check_rates <- function(model, rates_L, rates_D, dynamic_liquidity) {
  shares <- .release_gate_softmax(model, rates_L, rates_D)
  eq <- .release_gate_quantities_and_derivatives(model, shares)
  liquidity <- if (dynamic_liquidity) {
    .release_gate_pooled_liquidity(model, shares)
  } else {
    list(lambda_L = model@baseLambda_L, lambda_D = model@baseLambda_D)
  }
  pi_L <- eq$q_L / eq$Q_L[shares$row_bank]
  pi_D <- eq$q_D / eq$Q_D[shares$row_bank]
  rf_L <- vapply(seq_along(model@labels), function(j) {
    sum(pi_L[shares$row_bank == j] * model@riskFreeRate[shares$row_bank == j])
  }, numeric(1))
  rf_D <- vapply(seq_along(model@labels), function(j) {
    sum(pi_D[shares$row_bank == j] * model@riskFreeRate[shares$row_bank == j])
  }, numeric(1))
  cost_L <- vapply(seq_along(model@labels), function(j) {
    sum(pi_L[shares$row_bank == j] * model@costRaw_L[shares$row_bank == j])
  }, numeric(1))
  cost_D <- vapply(seq_along(model@labels), function(j) {
    sum(pi_D[shares$row_bank == j] * model@costRaw_D[shares$row_bank == j])
  }, numeric(1))

  implied_L <- rf_L + cost_L + eq$markup_L + liquidity$lambda_L + model@omega_L
  implied_D <- rf_D - eq$markdown_D + cost_D - liquidity$lambda_D + model@omega_D
  # Test returned equilibrium rates, not markups obtained by solving F itself.
  realized_markup_L <- rates_L - rf_L - cost_L - liquidity$lambda_L - model@omega_L
  realized_markdown_D <- rf_D + cost_D - liquidity$lambda_D + model@omega_D - rates_D
  foc_L <- max(abs(as.numeric(eq$F_L %*% realized_markup_L) + eq$Q_L)) / max(1, max(eq$Q_L))
  foc_D <- max(abs(as.numeric(eq$F_D %*% realized_markdown_D) - eq$Q_D)) / max(1, max(eq$Q_D))
  rate_error <- max(abs(c(rates_L - implied_L, rates_D - implied_D)))

  list(shares = shares, equilibrium = eq, liquidity = liquidity,
    implied_L = implied_L, implied_D = implied_D,
    foc_residual = max(foc_L, foc_D), rate_error = rate_error)
}

test_that("bank_fit/simulate satisfies independent Nash FOCs for competition-only and pooled-liquidity merger rates", {
  data <- list(
    bank_id = c("A", "B", "E", "B", "C", "E"),
    market_id = c("m1", "m1", "m1", "m2", "m2", "m2"),
    s_cond_L = c(0.55, 0.30, 0.15, 0.28, 0.50, 0.22),
    s_cond_D = c(0.22, 0.48, 0.30, 0.50, 0.27, 0.23),
    market_size_L = c(100, 100, 100, 135, 135, 135),
    market_size_D = c(120, 120, 120, 110, 110, 110),
    r_L = c(0.055, 0.061, 0.047, 0.052, 0.064, 0.049),
    r_D = c(0.017, 0.023, 0.028, 0.021, 0.016, 0.026),
    r_f = rep(0.03, 6),
    W_L = matrix(0, 6, 0), W_D = matrix(0, 6, 0),
    H_out_L = matrix(1, 6, 1), H_out_D = matrix(1, 6, 1),
    out_iv_L = rep(0, 6), out_iv_D = rep(0, 6),
    market_state_region_id = rep(1L, 6),
    group_id = c("A", "B", "E", "B", "C", "E"),
    Z_psi = matrix(c(0.1, 0.5, 0.3, 1.0), ncol = 1),
    equity = c(A = 10, B = 8, C = 13, E = 9),
    liquid_assets = c(A = 5, B = 4, C = 7, E = 4.5),
    assets = c(A = 1000, B = 50, C = 5, E = 300),
    control.equ = list(damping = 0.5, tol = 1e-9, max_iter = 400)
  )
  params <- c(
    alpha_bar_L = log(100), alpha_bar_D = log(100),
    sigma_alpha_state_L = 0, sigma_alpha_state_D = 0,
    `alpha_state_raw_L[1]` = 0, `alpha_state_raw_D[1]` = 0,
    `theta_out_L[1]` = 0, `theta_out_D[1]` = 0,
    alpha_out_L = -1.7, alpha_out_D = -1.7,
    log_phi_bar_L = log(15), log_phi_bar_D = log(15),
    eta_L = numeric(0), eta_D = numeric(0), gamma_psi = 1
  )

  fitted <- bank_fit(parameters = params, data = data)
  baseline <- fitted@model
  expect_identical(baseline@assets, unname(data$assets[baseline@labels]))
  expect_equal(length(baseline@labels), 4L)
  expect_setequal(baseline@labels, c("A", "B", "C", "E"))

  post <- c(A = "A", B = "A", C = "A", E = "E")
  simulated <- simulate(fitted, ownerPost = post)
  cf <- simulated@diagnostics$counterfactual
  expect_equal(cf$n_draws, 1L)

  take_rate <- function(bank, side) cf$rates[[side]][[bank]]$value
  comp_L <- vapply(baseline@labels, take_rate, numeric(1), side = "comp_L")
  comp_D <- vapply(baseline@labels, take_rate, numeric(1), side = "comp_D")
  full_L <- vapply(baseline@labels, take_rate, numeric(1), side = "full_L")
  full_D <- vapply(baseline@labels, take_rate, numeric(1), side = "full_D")
  baseline@ownerPost <- post[baseline@labels]

  ## The competition-only rates must use each bank's frozen calibrated
  ## liquidity wedge and omega, even though simulate() leaves @model@pricePost
  ## at the separate full-merger solution.
  comp_check <- .release_gate_check_rates(baseline, comp_L, comp_D, FALSE)
  expect_lt(comp_check$foc_residual, 1e-5)
  expect_lt(comp_check$rate_error, 1e-5)
  expect_equal(comp_check$liquidity$lambda_L, baseline@baseLambda_L, tolerance = 1e-12)
  expect_equal(comp_check$liquidity$lambda_D, baseline@baseLambda_D, tolerance = 1e-12)

  # Interior rates make equality FOCs appropriate rather than boundary KKT.
  expect_true(all(c(comp_L, full_L) > 1e-5 & c(comp_L, full_L) < .25 - 1e-5))
  expect_true(all(c(comp_D, full_D) > 1e-5 & c(comp_D, full_D) < .25 - 1e-5))
  full_check <- .release_gate_check_rates(baseline, full_L, full_D, TRUE)
  expect_gt(max(abs(full_check$liquidity$lambda_L - comp_check$liquidity$lambda_L)), 1e-4)
  expect_lt(full_check$foc_residual, 1e-5)
  expect_lt(full_check$rate_error, 1e-5)
  expect_equal(full_check$liquidity$lambda_L[baseline@ownerPost == "A"],
    rep(full_check$liquidity$lambda_L[baseline@ownerPost == "A"][1], 3), tolerance = 1e-12
  )
  expect_equal(full_check$liquidity$lambda_D[baseline@ownerPost == "A"],
    rep(full_check$liquidity$lambda_D[baseline@ownerPost == "A"][1], 3), tolerance = 1e-12
  )

  ## The returned convenience model stores full-merger rates; competition-only
  ## rates must be read from their own diagnostics entry.
  expect_equal(unname(simulated@model@pricePost[seq_len(4)]), unname(full_L), tolerance = 1e-12)
  expect_equal(unname(simulated@model@pricePost[4 + seq_len(4)]), unname(full_D), tolerance = 1e-12)
  expect_equal(unname(vapply(baseline@labels, function(b) cf$liquidity$lambda_full[[b]]$value,
    numeric(1))),
    0.5 * (full_check$liquidity$lambda_L + full_check$liquidity$lambda_D), tolerance = 1e-8
  )
})
