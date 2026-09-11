## Regression tests for the red-team-flagged psiRule = "asset_weighted" defect
## (2026-09): the migration had substituted `liquidAssets + equity` for
## simulate_merger_exposure.R's true weight, TOTAL bank assets
## (create_stability_data.R:275-276). Fixed via a new `assets` slot on
## BankMarket, wired through bank_fit(data = list(..., assets = ...)).

test_that("asset_weighted psiRule uses data$assets, not liquidAssets+equity, when supplied", {
  data <- .make_bank_test_data()
  ## Deliberately make liquidAssets+equity ORDER DIFFERENTLY from assets, so a
  ## silent liquidAssets+equity substitution would move the pooled psi in a
  ## detectably different direction than the true assets weighting.
  data$equity <- c(A = 10, B = 8, C = 12) # liquidAssets+equity: A=15,B=12,C=18 (A<B<C)
  data$liquid_assets <- c(A = 5, B = 4, C = 6)
  data$assets <- c(A = 500, B = 50, C = 5) # TRUE weight: A>>B>>C (reversed order)

  params <- .make_bank_test_params()
  fit <- bank_fit(parameters = params, data = data)
  expect_false(any(is.na(fit@model@assets)))

  ## Give A and B VISIBLY different psi directly (gamma_psi=0 in the shared
  ## test params makes every bank's calibrated psi identical, which would make
  ## any weighting choice trivially indistinguishable -- set it explicitly).
  fit@model@psi <- c(A = 0.02, B = 0.20, C = 0.10)
  fit@model@psiRule <- "asset_weighted"

  g <- c(which(fit@model@labels == "A"), which(fit@model@labels == "B"))
  ## No warning should fire: data$assets was supplied.
  expect_no_warning(used <- financial:::.bank_asset_weighted_psi(fit@model, g))

  weighted_true_assets <- stats::weighted.mean(fit@model@psi[g], w = pmax(data$assets[c("A", "B")], 1))
  weighted_proxy <- stats::weighted.mean(fit@model@psi[g], w = pmax(data$liquid_assets[c("A", "B")] + data$equity[c("A", "B")], 1))
  ## True-assets weighting (500 vs 50) pulls the result close to A's psi
  ## (0.02); the liquidAssets+equity proxy (15 vs 12, nearly equal) would
  ## instead land close to the midpoint -- the two must differ materially.
  expect_false(isTRUE(all.equal(weighted_true_assets, weighted_proxy, tolerance = 1e-3)))
  expect_equal(used, weighted_true_assets, tolerance = 1e-10)
  expect_lt(abs(used - fit@model@psi[["A"]]), abs(weighted_proxy - fit@model@psi[["A"]]))
})

test_that("asset_weighted psiRule falls back to liquidAssets+equity WITH an explicit warning when data$assets is absent", {
  data <- .make_bank_test_data() # no data$assets
  params <- .make_bank_test_params()
  fit <- bank_fit(parameters = params, data = data)
  expect_true(all(is.na(fit@model@assets)))

  g <- c(which(fit@model@labels == "A"), which(fit@model@labels == "B"))
  expect_warning(
    used <- financial:::.bank_asset_weighted_psi(fit@model, g),
    regexp = "asset_weighted.*liquidAssets \\+ equity|data\\$assets"
  )
  fallback <- stats::weighted.mean(fit@model@psi[g], w = pmax(fit@model@liquidAssets[g] + fit@model@equity[g], 1))
  expect_equal(used, fallback, tolerance = 1e-10)
})
