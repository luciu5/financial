# Capture pre-migration golden-master oracles for the Bank module
# (stability @ cc95e2e0). Sources ONLY the pure-R deterministic solver +
# closed-form Beta/liquidity primitives (counterfactual_solver.R). Does NOT
# touch Stan estimation or DB/FRED/QCEW ingestion (out of migration scope).
#
# READ-ONLY against stability source. Writes fixtures under tests/oracles/.
#
# Run with:
#   Rscript tests/oracles/capture_bank_oracles.R

STABILITY_SOLVER <- "/fsr/home/m1cst00/Projects/active/stability/code/counterfactual_solver.R"
OUT_DIR <- "/fsr/home/m1cst00/Projects/ioverse/financial/tests/oracles"
SEED <- 20260910L

cat("R version:", R.version.string, "\n")
cat("Sourcing:", STABILITY_SOLVER, "\n")

# Track any missing-object errors while sourcing (per task instructions).
source_result <- tryCatch({
  source(STABILITY_SOLVER)
  "OK"
}, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("Source result:", source_result, "\n")

required_objects <- c(
  "calc_portfolio_liquidity", "simulate_beta_portfolio_liquidity",
  "beta_tail_prob", "beta_tail_first_moment", "beta_tail_second_moment",
  "beta_lower_prob", "beta_lower_first_moment", "beta_lower_second_moment",
  "beta_inside_shapes", "beta_inside_mean", "beta_inside_var",
  "normal_excess_second_moment", "single_market_beta_excess_moments"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  cat("MISSING OBJECTS after sourcing counterfactual_solver.R alone:\n")
  print(missing_objects)
} else {
  cat("All required objects present after sourcing counterfactual_solver.R alone (no merger_common.R needed for these primitives).\n")
}

# ---------------------------------------------------------------------------
# Primary oracle: synthetic multi-market bank portfolio.
# calc_portfolio_liquidity() closed-form Normal approx (method="normal",
# forced, so multi-market always uses the Normal branch for comparability)
# vs simulate_beta_portfolio_liquidity() Monte-Carlo with 200,000 draws.
# ---------------------------------------------------------------------------
cat("\n=== Primary oracle: MC vs closed-form, multi-market portfolio ===\n")

set.seed(SEED)
n_markets <- 5
market_id <- 1:n_markets
market_size_L <- runif(n_markets, 50, 200)
market_size_D <- runif(n_markets, 50, 200)
s_cond_L <- runif(n_markets, 0.05, 0.4)
s_cond_D <- runif(n_markets, 0.05, 0.4)
outside_mu_L <- runif(n_markets, 0.3, 0.7)
outside_mu_D <- runif(n_markets, 0.3, 0.7)
phi_out_L <- runif(n_markets, 5, 30)
phi_out_D <- runif(n_markets, 5, 30)
equity <- 10
liquid_assets <- 5
psi <- 2

primary_inputs <- list(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = phi_out_L, phi_out_D = phi_out_D,
  equity = equity, liquid_assets = liquid_assets, psi = psi,
  market_id = market_id, seed = SEED, n_markets = n_markets,
  mc_draws = 200000L
)

cf_result <- calc_portfolio_liquidity(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = phi_out_L, phi_out_D = phi_out_D,
  equity = equity, liquid_assets = liquid_assets, psi = psi,
  market_id = market_id, method = "normal"
)

mc_result <- simulate_beta_portfolio_liquidity(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = phi_out_L, phi_out_D = phi_out_D,
  equity = equity, liquid_assets = liquid_assets, psi = psi,
  market_id = market_id, draws = 200000L, seed = SEED
)

cat("Closed-form expected_liq_penalty:", cf_result$expected_liq_penalty, "\n")
cat("Monte-Carlo expected_liq_penalty:", mc_result$expected_liq_penalty, "\n")
cat("Absolute diff:", abs(cf_result$expected_liq_penalty - mc_result$expected_liq_penalty), "\n")
cat("Relative diff:", abs(cf_result$expected_liq_penalty - mc_result$expected_liq_penalty) / cf_result$expected_liq_penalty, "\n")
cat("Closed-form lambda_liq:", cf_result$lambda_liq, " | MC lambda_liq:", mc_result$lambda_liq, "\n")

primary_oracle <- list(
  inputs = primary_inputs,
  closed_form = cf_result,
  monte_carlo = mc_result,
  comparison = list(
    cf_expected_liq_penalty = cf_result$expected_liq_penalty,
    mc_expected_liq_penalty = mc_result$expected_liq_penalty,
    abs_diff = abs(cf_result$expected_liq_penalty - mc_result$expected_liq_penalty),
    rel_diff = abs(cf_result$expected_liq_penalty - mc_result$expected_liq_penalty) / cf_result$expected_liq_penalty,
    cf_lambda_liq = cf_result$lambda_liq,
    mc_lambda_liq = mc_result$lambda_liq
  )
)
saveRDS(primary_oracle, file.path(OUT_DIR, "bank_liquidity_mc_vs_cf.rds"))

primary_df <- data.frame(
  metric = c("expected_liq_penalty", "lambda_liq"),
  closed_form = c(cf_result$expected_liq_penalty, cf_result$lambda_liq),
  monte_carlo = c(mc_result$expected_liq_penalty, mc_result$lambda_liq)
)
write.csv(primary_df, file.path(OUT_DIR, "bank_liquidity_mc_vs_cf.csv"), row.names = FALSE)
cat("Saved bank_liquidity_mc_vs_cf.rds + .csv\n")

# ---------------------------------------------------------------------------
# Limiting cases
# ---------------------------------------------------------------------------
cat("\n=== Limiting case (a): phi -> very large (near-zero variance) ===\n")
# As phi -> Inf, beta_inside_var -> 0, so sigma_w collapses toward the 1e-10
# variance floor in calc_portfolio_liquidity. expected_liq_penalty should
# approach the deterministic shortfall 0.5*psi*max(mu_W - buffer,0)^2, where
# buffer == liquid_assets in this codebase's naming.
phi_huge <- 1e8
la_case <- calc_portfolio_liquidity(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = rep(phi_huge, n_markets), phi_out_D = rep(phi_huge, n_markets),
  equity = equity, liquid_assets = liquid_assets, psi = psi,
  market_id = market_id, method = "normal"
)
deterministic_shortfall <- 0.5 * psi * max(la_case$mu_W - liquid_assets, 0)^2
cat("mu_W:", la_case$mu_W, " sigma_W:", la_case$sigma_W, "\n")
cat("calc_portfolio_liquidity expected_liq_penalty:", la_case$expected_liq_penalty, "\n")
cat("Deterministic shortfall 0.5*psi*max(mu_W-buffer,0)^2:", deterministic_shortfall, "\n")
cat("Abs diff:", abs(la_case$expected_liq_penalty - deterministic_shortfall), "\n")

limiting_phi_large <- list(
  inputs = list(phi = phi_huge, psi = psi, liquid_assets = liquid_assets,
                market_size_L = market_size_L, market_size_D = market_size_D,
                s_cond_L = s_cond_L, s_cond_D = s_cond_D,
                outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
                equity = equity, market_id = market_id),
  result = la_case,
  deterministic_shortfall_formula = deterministic_shortfall,
  abs_diff = abs(la_case$expected_liq_penalty - deterministic_shortfall),
  note = "phi -> 1e8 (near-zero variance); expected_liq_penalty should approach 0.5*psi*max(mu_W-liquid_assets,0)^2"
)
saveRDS(limiting_phi_large, file.path(OUT_DIR, "bank_limiting_phi_large.rds"))
cat("Saved bank_limiting_phi_large.rds\n")

cat("\n=== Limiting case (b): psi = 0 -> zero liquidity cost ===\n")
psi_zero_case <- calc_portfolio_liquidity(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = phi_out_L, phi_out_D = phi_out_D,
  equity = equity, liquid_assets = liquid_assets, psi = 0,
  market_id = market_id, method = "normal"
)
cat("expected_liq_penalty (psi=0):", psi_zero_case$expected_liq_penalty, "\n")
cat("lambda_liq (psi=0):", psi_zero_case$lambda_liq, "\n")

limiting_psi_zero <- list(
  inputs = list(psi = 0, liquid_assets = liquid_assets,
                market_size_L = market_size_L, market_size_D = market_size_D,
                s_cond_L = s_cond_L, s_cond_D = s_cond_D,
                outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
                phi_out_L = phi_out_L, phi_out_D = phi_out_D,
                equity = equity, market_id = market_id),
  result = psi_zero_case,
  note = "psi = 0 should give exactly zero expected_liq_penalty and lambda_liq regardless of mu_W/sigma_W"
)
saveRDS(limiting_psi_zero, file.path(OUT_DIR, "bank_limiting_psi_zero.rds"))
cat("Saved bank_limiting_psi_zero.rds\n")

cat("\n=== Limiting case (c): single-market exact Beta branch vs multi-market Normal approx ===\n")
# Single market: hybrid method (default) dispatches to the EXACT
# single_market_beta_excess_moments() branch. Compare against forcing the
# Normal approximation on the SAME single-market inputs, and also show the
# genuinely multi-market Normal-approx result for contrast.
single_market_id <- rep(1, n_markets)  # collapse all "markets" into ONE market

single_hybrid <- calc_portfolio_liquidity(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = phi_out_L, phi_out_D = phi_out_D,
  equity = equity, liquid_assets = liquid_assets, psi = psi,
  market_id = single_market_id, method = "hybrid"
)
single_normal <- calc_portfolio_liquidity(
  market_size_L = market_size_L, market_size_D = market_size_D,
  s_cond_L = s_cond_L, s_cond_D = s_cond_D,
  outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
  phi_out_L = phi_out_L, phi_out_D = phi_out_D,
  equity = equity, liquid_assets = liquid_assets, psi = psi,
  market_id = single_market_id, method = "normal"
)
multi_normal <- cf_result  # already computed above with market_id = 1:n_markets

cat("Single-market EXACT (hybrid) method:", single_hybrid$liquidity_method, " expected_liq_penalty:", single_hybrid$expected_liq_penalty, "\n")
cat("Single-market NORMAL-forced method:", single_normal$liquidity_method, " expected_liq_penalty:", single_normal$expected_liq_penalty, "\n")
cat("Multi-market NORMAL method:", multi_normal$liquidity_method, " expected_liq_penalty:", multi_normal$expected_liq_penalty, "\n")

limiting_single_vs_multi <- list(
  inputs = list(market_size_L = market_size_L, market_size_D = market_size_D,
                s_cond_L = s_cond_L, s_cond_D = s_cond_D,
                outside_mu_L = outside_mu_L, outside_mu_D = outside_mu_D,
                phi_out_L = phi_out_L, phi_out_D = phi_out_D,
                equity = equity, liquid_assets = liquid_assets, psi = psi),
  single_market_exact_hybrid = single_hybrid,
  single_market_normal_forced = single_normal,
  multi_market_normal = multi_normal,
  note = paste(
    "single_market_exact_hybrid uses calc_portfolio_liquidity(method='hybrid')",
    "on market_id collapsed to a single market -> dispatches to",
    "single_market_beta_excess_moments() (liquidity_method =='single_market_beta_exact').",
    "single_market_normal_forced uses method='normal' on the SAME single-market",
    "collapsed inputs for a same-data comparison of exact vs Normal approx.",
    "multi_market_normal is calc_portfolio_liquidity(method='normal') on the",
    "original 5-market inputs (== primary oracle closed_form result)."
  )
)
saveRDS(limiting_single_vs_multi, file.path(OUT_DIR, "bank_limiting_single_vs_multi_market.rds"))
cat("Saved bank_limiting_single_vs_multi_market.rds\n")

limiting_df <- data.frame(
  case = c("phi_large_calc", "phi_large_deterministic_formula",
           "psi_zero_expected_liq_penalty", "psi_zero_lambda_liq",
           "single_market_exact", "single_market_normal_forced", "multi_market_normal"),
  value = c(la_case$expected_liq_penalty, deterministic_shortfall,
            psi_zero_case$expected_liq_penalty, psi_zero_case$lambda_liq,
            single_hybrid$expected_liq_penalty, single_normal$expected_liq_penalty, multi_normal$expected_liq_penalty)
)
write.csv(limiting_df, file.path(OUT_DIR, "bank_limiting_cases_manifest.csv"), row.names = FALSE)
cat("Saved bank_limiting_cases_manifest.csv\n")

# ---------------------------------------------------------------------------
# Beta-moment primitive grid: the SHARED primitive
# (beta_tail_prob, beta_tail_first_moment, beta_tail_second_moment,
#  beta_lower_prob, beta_lower_first_moment, beta_lower_second_moment)
# over a grid of (cutoff, shape1, shape2).
# ---------------------------------------------------------------------------
cat("\n=== Beta-moment primitive grid ===\n")

cutoffs <- c(-0.5, 0, 0.001, 0.1, 0.25, 0.5, 0.75, 0.9, 0.999, 1, 1.5)
shape1s <- c(0.5, 1, 2, 4, 10)
shape2s <- c(0.5, 1, 2, 4, 10)

beta_grid <- expand.grid(cutoff = cutoffs, shape1 = shape1s, shape2 = shape2s)
beta_grid$tail_prob <- with(beta_grid, beta_tail_prob(cutoff, shape1, shape2))
beta_grid$tail_first_moment <- with(beta_grid, beta_tail_first_moment(cutoff, shape1, shape2))
beta_grid$tail_second_moment <- with(beta_grid, beta_tail_second_moment(cutoff, shape1, shape2))
beta_grid$lower_prob <- with(beta_grid, beta_lower_prob(cutoff, shape1, shape2))
beta_grid$lower_first_moment <- with(beta_grid, beta_lower_first_moment(cutoff, shape1, shape2))
beta_grid$lower_second_moment <- with(beta_grid, beta_lower_second_moment(cutoff, shape1, shape2))

cat("Grid dimensions:", nrow(beta_grid), "rows x", ncol(beta_grid), "cols\n")
cat("Any non-finite values?", any(!is.finite(as.matrix(beta_grid))), "\n")

saveRDS(beta_grid, file.path(OUT_DIR, "beta_moments_grid.rds"))
write.csv(beta_grid, file.path(OUT_DIR, "beta_moments_grid.csv"), row.names = FALSE)
cat("Saved beta_moments_grid.rds + .csv\n")

# also capture the inside-shape helpers (beta_inside_mean/var/shapes),
# which feed the liquidity primitives above and are part of the same
# shared-private surface.
inside_grid <- expand.grid(mu = c(0.05, 0.2, 0.5, 0.8, 0.95), phi = c(1, 5, 20, 100, 1000))
inside_grid$mean_val <- with(inside_grid, beta_inside_mean(mu))
inside_grid$var_val <- with(inside_grid, beta_inside_var(mu, phi))
shapes <- beta_inside_shapes(inside_grid$mu, inside_grid$phi)
inside_grid$shape1 <- shapes$shape1
inside_grid$shape2 <- shapes$shape2

saveRDS(inside_grid, file.path(OUT_DIR, "beta_inside_shapes_grid.rds"))
write.csv(inside_grid, file.path(OUT_DIR, "beta_inside_shapes_grid.csv"), row.names = FALSE)
cat("Saved beta_inside_shapes_grid.rds + .csv\n")

cat("\n=== Bank module oracle capture complete ===\n")
