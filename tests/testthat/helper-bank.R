## Shared synthetic-data helpers for the bank-module test files
## (test-bank-parity.R, test-bank-assets.R). testthat auto-sources
## helper-*.R files before running any test file, so these are visible
## everywhere without re-defining them per file.

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
