## Regression test: the shared partial-Beta moment helpers must reproduce the
## behavioral oracle captured from stability@cc95e2e0 BEFORE migration
## (tests/oracles/beta_moments_grid.rds). This guards the one piece of shared
## numeric infrastructure between the debt and banking modules.

test_that("shared beta-moment helpers reproduce the pre-migration oracle exactly", {
  grid_path <- testthat::test_path("..", "oracles", "beta_moments_grid.rds")
  skip_if_not(file.exists(grid_path), "beta_moments_grid.rds oracle not present")
  grid <- readRDS(grid_path)

  pairs <- list(
    tail_prob           = financial:::beta_tail_prob,
    tail_first_moment   = financial:::beta_tail_first_moment,
    tail_second_moment  = financial:::beta_tail_second_moment,
    lower_prob          = financial:::beta_lower_prob,
    lower_first_moment  = financial:::beta_lower_first_moment,
    lower_second_moment = financial:::beta_lower_second_moment
  )

  for (col in names(pairs)) {
    got <- pairs[[col]](grid$cutoff, grid$shape1, grid$shape2)
    expect_equal(got, grid[[col]], tolerance = 1e-12, info = col)
  }
})

test_that("debt h_focal integral is the share-derivative-weighted lower partial moment", {
  # h_focal (MergerDebt) integrates the logit-share-derivative kernel x*(1-x) over
  # [0, u] against a Beta density -> it equals E[X(1-X)1{X<=u}] = lower_first - lower_second.
  # This ties the debt-module primitive to the banking-module partial moments
  # (the shared-code justification), and explains WHY it appears in share reshaping.
  u <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  s1 <- 4; s2 <- 4
  h <- financial:::beta_h_focal_integral(u, s1, s2)
  identity <- financial:::beta_lower_first_moment(u, s1, s2) -
    financial:::beta_lower_second_moment(u, s1, s2)
  expect_equal(h, identity, tolerance = 1e-10)

  # independent numeric-integration oracle for the same quantity
  num <- vapply(u, function(uu) {
    stats::integrate(function(x) x * (1 - x) * stats::dbeta(x, s1, s2), 0, uu)$value
  }, numeric(1))
  expect_equal(h, num, tolerance = 1e-8)
})
