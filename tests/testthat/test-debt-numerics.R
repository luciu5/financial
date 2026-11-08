# A (permanent): independent moments catch support, concentrated-density,
# focal-index and multidimensional callback errors that FOCs conditional on
# calcShares() cannot expose.
moments <- function(...) financial:::.debt_survival_moments(...)

test_that("A: zero debt and extreme Beta shapes retain analytic partial moments", {
    for (shape in list(c(1, 1), c(4, 40), c(5000, 5000))) {
        z <- moments(c(0, 3), 0, shape)
        expected <- shape[1] / (sum(shape) + 1)
        expect_equal(z$h / z$g, rep(expected, 2), tolerance = 1e-10)
        expect_equal(financial:::beta_g_focal_integral(1, shape[1], shape[2]),
            shape[2] / sum(shape), tolerance = 1e-12)
        expect_equal(financial:::beta_h_focal_integral(1, shape[1], shape[2]),
            prod(shape / sum(shape)) * sum(shape) / (sum(shape) + 1),
            tolerance = 1e-12)
    }
})

test_that("A: solvent moments match independent clipped double integrals and focal permutations", {
    profit <- c(3, 7)
    debt <- 5
    for (shape in list(c(1, 1), c(4, 40))) {
        oracle <- function(m, power) {
            integrate(function(v) vapply(v, function(u2) {
                cutoff <- max(0, min(1, (sum(profit) - debt - profit[2] * u2) / profit[1]))
                if (cutoff == 0) return(0)
                integrate(function(u1) {
                    u <- if (m == 1) u1 else u2
                    u^power * (1 - u) * dbeta(u1, shape[1], shape[2]) *
                        dbeta(u2, shape[1], shape[2])
                }, 0, cutoff, rel.tol = 1e-10)$value
            }, numeric(1)), 0, 1, rel.tol = 1e-9, subdivisions = 1000)$value
        }
        z <- moments(profit, debt, shape)
        expect_equal(z$g, vapply(1:2, oracle, numeric(1), power = 0), tolerance = 1e-7)
        expect_equal(z$h, vapply(1:2, oracle, numeric(1), power = 1), tolerance = 1e-7)
        swapped <- moments(rev(profit), debt, shape, focal = 2)
        expect_equal(z$h / z$g, rev(swapped$h / swapped$g), tolerance = 1e-7)
        expect_equal(z$h / z$g, moments(profit, debt, shape, focal = 2)$h /
            moments(profit, debt, shape, focal = 2)$g, tolerance = 1e-7)
    }
    # A zero-profit focal market remains a meaningful unconditional coordinate.
    z <- moments(c(0, 7), 3, c(2, 3), focal = 1)
    expect_equal(z$h[1] / z$g[1], 2 / 6, tolerance = 1e-7)
})

test_that("A: three-market uniform cubature matches an independent simplex identity", {
    # sum(U)<1 has probability 1/6. On this simplex E[U_i]=1/4,
    # E[U_i^2]=1/10, hence g=1/8 and h=1/40 for each market.
    z <- moments(rep(1, 3), 2, c(1, 1),
        control = list(method = "hcubature", maxEval = 1e6))
    expect_equal(z$g, rep(1 / 8, 3), tolerance = 1e-7)
    expect_equal(z$h, rep(1 / 40, 3), tolerance = 1e-7)
    expect_error(moments(rep(1, 3), 2.1, c(2.3, 3.7),
        control = list(method = "hcubature", maxEval = 1)),
        class = "financial_integration_failure")
})

test_that("A: undefined conditioning and invalid economics cannot become zero share roots", {
    expect_error(moments(c(2, 3), 5, c(2, 3)), class = "financial_zero_survival")
    zero <- moments(c(0, 0), 0, c(2, 3))
    expect_equal(zero$h / zero$g, rep(1 / 3, 2), tolerance = 1e-10)
    expect_error(moments(c(0, 0), 1, c(2, 3)), class = "financial_zero_survival")
    expect_error(moments(c(NA, 3), 1, c(2, 3)), class = "financial_equilibrium_failure")
    expect_error(moments(c(2, 3), 1, c(-1, 3)), class = "financial_equilibrium_failure")
})


test_that("A: missing debt margins fail explicitly instead of deleting firm-profit information", {
    args <- list(prices = rep(10, 5), shares = rep(.2, 5),
        margins = c(NA, rep(.5, 4)), ownerPre = as.character(1:5),
        ownerPost = as.character(1:5))
    expect_error(do.call(logit.debt, args), "product positions: 1",
        class = "financial_equilibrium_failure")
    args$margins[1] <- Inf
    expect_error(do.call(logit.debt, args), "product positions: 1",
        class = "financial_equilibrium_failure")
})
