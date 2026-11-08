## shared-private: partial/incomplete-Beta moment helpers
##
## This is the ONLY genuinely shared numeric infrastructure between the debt and
## banking modules (see design/ARCHITECTURE.md). Both families integrate a
## financial-risk quantity over a Beta-distributed share on [0, 1]:
##   - debt module: outside/rival share u ~ Beta -> firm default threshold s0Focal,
##     reshaping conditional logit shares via incomplete-Beta moments.
##   - banking module: participation share ~ Beta -> aggregated liquidity shortfall,
##     priced via lower/upper (tail) partial moments of the shortfall distribution.
## The ECONOMIC USES diverge; only the [0,1] partial-Beta primitive is shared.
##
## These functions are INTERNAL (not exported). Definitions are preserved verbatim
## from stability/code/counterfactual_solver.R (SHA cc95e2e0) so that the migrated
## bank solver matches the captured behavioral oracle (tests/oracles/beta_moments_grid.rds)
## bit-for-bit, and the debt-style incomplete-Beta moments (h/g integrals) mirror
## MergerDebt/R/DebtFunctions.R (SHA ea727430).
##
## @keywords internal
## @noRd

#' @include FinancialArchitecture.R
NULL

## ---- mean-precision (mu, phi) parameterization (banking participation shares) ----

## Mean of the inside (1 - outside) share under the banking convention.
beta_inside_mean <- function(mu) {
  1 - mu
}

beta_inside_second <- function(mu, phi) {
  a <- pmax(mu * phi, 1e-6)
  b <- pmax((1 - mu) * phi, 1e-6)
  b * (b + 1) / ((a + b) * (a + b + 1))
}

beta_inside_var <- function(mu, phi) {
  a <- pmax(mu * phi, 1e-6)
  b <- pmax((1 - mu) * phi, 1e-6)
  a * b / ((a + b)^2 * (a + b + 1))
}

## Map (mu, phi) to the (shape1, shape2) used by the partial-moment helpers below.
## Note the swap: shape1 corresponds to the inside mass (1 - mu).
beta_inside_shapes <- function(mu, phi) {
  list(
    shape1 = pmax((1 - mu) * phi, 1e-6),
    shape2 = pmax(mu * phi, 1e-6)
  )
}

## ---- partial moments on [cutoff, 1] (upper tail) ----

beta_tail_prob <- function(cutoff, shape1, shape2) {
  ifelse(cutoff <= 0, 1, ifelse(cutoff >= 1, 0, 1 - pbeta(cutoff, shape1, shape2)))
}

beta_tail_first_moment <- function(cutoff, shape1, shape2) {
  base <- shape1 / (shape1 + shape2)
  ifelse(cutoff <= 0, base, ifelse(cutoff >= 1, 0, base * (1 - pbeta(cutoff, shape1 + 1, shape2))))
}

beta_tail_second_moment <- function(cutoff, shape1, shape2) {
  base <- shape1 * (shape1 + 1) / ((shape1 + shape2) * (shape1 + shape2 + 1))
  ifelse(cutoff <= 0, base, ifelse(cutoff >= 1, 0, base * (1 - pbeta(cutoff, shape1 + 2, shape2))))
}

## ---- partial moments on [0, cutoff] (lower) ----

beta_lower_prob <- function(cutoff, shape1, shape2) {
  ifelse(cutoff <= 0, 0, ifelse(cutoff >= 1, 1, pbeta(cutoff, shape1, shape2)))
}

beta_lower_first_moment <- function(cutoff, shape1, shape2) {
  base <- shape1 / (shape1 + shape2)
  ifelse(cutoff <= 0, 0, ifelse(cutoff >= 1, base, base * pbeta(cutoff, shape1 + 1, shape2)))
}

beta_lower_second_moment <- function(cutoff, shape1, shape2) {
  base <- shape1 * (shape1 + 1) / ((shape1 + shape2) * (shape1 + shape2 + 1))
  ifelse(cutoff <= 0, 0, ifelse(cutoff >= 1, base, base * pbeta(cutoff, shape1 + 2, shape2)))
}

## ---- incomplete-Beta moment integrals (debt module h/g integrals) ----
##
## Preserved from MergerDebt hFocalIntegral / gFocalIntegral. These are the same
## partial-Beta-moment primitive expressed via the beta() normalization:
##   h(u) = E[X * 1{X <= u}] shape-shifted form = beta(s1+1,s2+1)/beta(s1,s2) * pbeta(u, s1+1, s2+1)
##   g(u) = beta(s1,s2+1)/beta(s1,s2) * pbeta(u, s1, s2+1)
## Algebraic shape prefactors replace the ratios of Beta functions so large
## valid shapes do not yield 0/0. These are the same truncated moments; the
## banking helpers and their economic parameterization remain unchanged.

beta_h_focal_integral <- function(u, shape1, shape2) {
  (shape1 / (shape1 + shape2)) * (shape2 / (shape1 + shape2 + 1)) *
    pbeta(u, shape1 = shape1 + 1, shape2 = shape2 + 1, lower.tail = TRUE)
}

beta_g_focal_integral <- function(u, shape1, shape2) {
  shape2 / (shape1 + shape2) *
    pbeta(u, shape1 = shape1, shape2 = shape2 + 1, lower.tail = TRUE)
}
