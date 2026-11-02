## Parity regression tests for the migrated debt module.
##
## Rule of use (tests/oracles/README.md): the migrated `financial` code must
## be tested AGAINST THE PRE-MIGRATION FIXTURES captured from legacy
## MergerDebt (SHA ea727430) execution, never against itself.

grid_path <- testthat::test_path("..", "oracles", "debt_goldenmaster_grid.rds")
sanity_path <- testthat::test_path("..", "oracles", "debt_zerodebt_direction_sanity.rds")
closedform_path <- testthat::test_path("..", "oracles", "debt_singlemarket_closedform.rds")

## ---------------------------------------------------------------------
## 1. Golden-master grid: valid equilibrium cases plus diagnosed traps.
## ---------------------------------------------------------------------

debt_grid_args <- function(inputs) {
    args <- list(
        prices = inputs$prices, shares = inputs$shares, margins = inputs$margins,
        ownerPre = inputs$ownerPre, ownerPost = inputs$ownerPost,
        debtPre = inputs$debtPre, debtPost = inputs$debtPost,
        marketID = inputs$marketID, mktSize = inputs$mktSize,
        density = inputs$density
    )
    if (!is.null(inputs$shareOutParm) && !all(is.na(inputs$shareOutParm))) {
        args$shareOutParm <- inputs$shareOutParm
    }
    args
}

test_that("valid debt grid cases retain captured migration parity [B: temporary]", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    skip_if_not(file.exists(grid_path), "debt_goldenmaster_grid.rds oracle not present")
    grid <- readRDS(grid_path)

    ## B sunset: remove or replace this raw-grid layer by 2027-11-01. Earlier
    ## removal is appropriate if the capture R/antitrust toolchain becomes
    ## available and the independent economic checks below cover its material
    ## cases. This parity layer catches cross-version migration drift while
    ## the historical oracle remains useful.
    tol <- 1e-6
    for (case_name in setdiff(names(grid), c("zero_uniform", "low_beta", "high_beta"))) {
        fixture <- grid[[case_name]]
        inputs <- fixture$inputs
        set.seed(inputs$seed)
        fit <- do.call(logit.debt, debt_grid_args(inputs))

        expect_equal(as.numeric(fit@pricePre), as.numeric(fixture$pricePre),
            tolerance = tol, info = paste(case_name, "pricePre"))
        expect_equal(as.numeric(fit@pricePost), as.numeric(fixture$pricePost),
            tolerance = tol, info = paste(case_name, "pricePost"))
        expect_equal(as.numeric(fit@shareOutParm), as.numeric(fixture$shareOutParm_calibrated),
            tolerance = tol, info = paste(case_name, "shareOutParm"))

        sharesPre <- calcShares(fit, preMerger = TRUE)
        sharesPost <- calcShares(fit, preMerger = FALSE)
        expect_equal(as.numeric(sharesPre), as.numeric(fixture$sharesPre),
            tolerance = tol, info = paste(case_name, "sharesPre"))
        expect_equal(as.numeric(sharesPost), as.numeric(fixture$sharesPost),
            tolerance = tol, info = paste(case_name, "sharesPost"))
    }
})

test_that("low-debt calibration preserves the identified Beta law [A: permanent]", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    skip_if_not(file.exists(grid_path), "debt_goldenmaster_grid.rds oracle not present")
    fixture <- readRDS(grid_path)[["low_beta"]]
    inputs <- fixture$inputs
    set.seed(inputs$seed)
    fit <- do.call(logit.debt, debt_grid_args(inputs))

    ## Material failure caught: optimizer-version drift can change the
    ## calibrated price sensitivity or the economically relevant distribution
    ## of default thresholds while leaving the constructor/API unchanged.
    expect_equal(as.numeric(fit@slopes$alpha), as.numeric(fixture$slopes$alpha),
        tolerance = 1e-6)

    fitted_shapes <- fit@shareOutParm
    captured_shapes <- fixture$shareOutParm_calibrated
    beta_moments <- function(shape) {
        c(mean = shape[1] / sum(shape),
            variance = prod(shape) / (sum(shape)^2 * (sum(shape) + 1)))
    }
    expect_equal(unname(beta_moments(fitted_shapes)),
        unname(beta_moments(captured_shapes)), tolerance = 1e-6)

    ## Shapes are a parameterization; compare their observable CDF at
    ## economically relevant default-threshold cutoffs using the same
    ## 1e-6 criterion. This distinguishes a harmless optimizer-level change
    ## in raw shapes from a material change in default probabilities.
    cutoffs <- c(0.05, 0.10, 0.25, 0.50)
    fitted_cdf <- stats::pbeta(cutoffs, fitted_shapes[1], fitted_shapes[2])
    captured_cdf <- stats::pbeta(cutoffs, captured_shapes[1], captured_shapes[2])
    expect_equal(as.numeric(fitted_cdf), as.numeric(captured_cdf), tolerance = 1e-6)

    ## The outside-share integrals for this historical low-debt case produce
    ## NaN for the two merging firms, so their adjusted shares/prices are not
    ## valid equilibrium targets. Preserve the independently identified
    ## conditional-logit identity: each market's conditional inside shares
    ## sum to one and recover the normalized observed shares.
    n_markets <- length(unique(fit@marketID))
    conditional <- matrix(calcShares(fit, preMerger = TRUE, insideOnly = TRUE),
        nrow = n_markets)
    observed <- matrix(fit@shares, nrow = n_markets)
    observed <- observed / rowSums(observed)
    expect_true(all(is.finite(conditional)))
    expect_equal(rowSums(conditional), rep(1, n_markets), tolerance = 1e-6)
    expect_equal(unname(conditional), unname(observed), tolerance = 1e-6)
})

test_that("high-debt Beta calibration primitives are checked without equilibrium claims [A: permanent]", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    skip_if_not(file.exists(grid_path), "debt_goldenmaster_grid.rds oracle not present")
    fixture <- readRDS(grid_path)[["high_beta"]]
    inputs <- fixture$inputs
    set.seed(inputs$seed)
    fit <- do.call(logit.debt, debt_grid_args(inputs))

    ## The high-beta equilibrium outputs are C (legacy trap): NaN adjusted
    ## shares and unchanged post prices are not valid targets. This A test is
    ## calibration-only. It catches changes to slopes or the default-
    ## threshold distribution that constructor or input checks would miss.
    expect_equal(as.numeric(fit@slopes$alpha), as.numeric(fixture$slopes$alpha),
        tolerance = 1e-6)
    beta_moments <- function(shape) {
        c(mean = shape[1] / sum(shape),
            variance = prod(shape) / (sum(shape)^2 * (sum(shape) + 1)))
    }
    expect_equal(unname(beta_moments(fit@shareOutParm)),
        unname(beta_moments(fixture$shareOutParm_calibrated)), tolerance = 1e-6)

    cutoffs <- c(0.005, 0.01, 0.025, 0.05)
    fitted_cdf <- stats::pbeta(cutoffs, fit@shareOutParm[1], fit@shareOutParm[2])
    captured_cdf <- stats::pbeta(cutoffs,
        fixture$shareOutParm_calibrated[1], fixture$shareOutParm_calibrated[2])
    expect_equal(as.numeric(fitted_cdf), as.numeric(captured_cdf), tolerance = 1e-6)

    ## Independent conditional-logit identity: finite within-market shares
    ## add to one and equal softmax(meanval + alpha * price). This validates
    ## demand calibration only, not the invalid outside-adjusted equilibrium.
    n_markets <- length(unique(fit@marketID))
    conditional <- matrix(calcShares(fit, preMerger = TRUE, insideOnly = TRUE),
        nrow = n_markets)
    n_products <- ncol(conditional)
    alpha_by_product <- rep(fit@slopes$alpha, times = n_products)
    eta <- matrix(fit@slopes$meanval + alpha_by_product * fit@pricePre,
        nrow = n_markets)
    logit_shares <- exp(eta)
    logit_shares <- logit_shares / rowSums(logit_shares)
    expect_true(all(is.finite(conditional)))
    expect_equal(rowSums(conditional), rep(1, n_markets), tolerance = 1e-6)
    expect_equal(unname(conditional), unname(logit_shares), tolerance = 1e-6)
})

test_that("zero-debt uniform legacy trap is reported as nonconverged [C]", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    skip_if_not(file.exists(grid_path), "debt_goldenmaster_grid.rds oracle not present")
    inputs <- readRDS(grid_path)[["zero_uniform"]]$inputs
    set.seed(inputs$seed)

    ## C (legacy trap): BBsolve historically reports lack of improvement on
    ## this nonconvergent fixture. The warning is the diagnostic; do not
    ## freeze its invalid prices or NaN/default-adjusted shares as economics.
    expect_warning(
        invisible(do.call(logit.debt, debt_grid_args(inputs))),
        regexp = "BBsolve|converge|improvement",
        info = "zero_uniform is excluded from equilibrium-output parity"
    )
})

test_that("multimarket debt prices satisfy independent Bertrand FOCs [A: permanent]", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    skip_if_not(file.exists(grid_path), "debt_goldenmaster_grid.rds oracle not present")
    inputs <- readRDS(grid_path)[["low_uniform"]]$inputs
    set.seed(inputs$seed)
    fit <- do.call(logit.debt, debt_grid_args(inputs))

    foc_residual <- function(object, pre_merger) {
        prices <- if (pre_merger) object@pricePre else object@pricePost
        mc <- if (pre_merger) object@mcPre else object@mcPost
        owner <- if (pre_merger) object@ownerPre else object@ownerPost
        shares <- calcShares(object, preMerger = pre_merger)
        n_markets <- length(unique(object@marketID))
        n_products <- ncol(matrix(shares, nrow = n_markets))

        ## Independently aggregate public adjusted shares and apply the
        ## specified markup equation. This guards ownership/solver integration
        ## conditional on calcShares, not the default integrals themselves.
        product_shares <- matrix(shares, nrow = n_markets)
        firm_shares <- product_shares
        firm_by_product <- matrix(owner, nrow = n_markets)[1, ]
        for (j in seq_len(n_products)) {
            firm_shares[, j] <- rowSums(
                product_shares[, firm_by_product == firm_by_product[j], drop = FALSE]
            )
        }
        alpha <- matrix(object@slopes$alpha, nrow = n_markets, ncol = n_products)
        markup <- -1 / (alpha * (1 - firm_shares))
        as.numeric(prices - mc - as.numeric(markup))
    }

    for (pre_merger in c(TRUE, FALSE)) {
        residual <- foc_residual(fit, pre_merger)
        expect_true(all(is.finite(residual)))
        expect_true(max(abs(residual)) < 1e-6,
            info = if (pre_merger) "pre-merger Bertrand FOCs" else "post-merger Bertrand FOCs")
    }
})

## ---------------------------------------------------------------------
## 2. Single-market closed form: s0Focal = 1 - debt/profit.
## ---------------------------------------------------------------------

test_that("single-market s0Focal satisfies the closed-form debt identity [A: permanent]", {
    skip_if_not(file.exists(closedform_path), "debt_singlemarket_closedform.rds oracle not present")
    cf <- readRDS(closedform_path)
    inputs <- cf$inputs

    set.seed(inputs$seed)
    fit <- logit.debt(
        inputs$prices, inputs$shares, inputs$margins, inputs$ownerPre, inputs$ownerPost,
        debtPre = inputs$debtPre, debtPost = inputs$debtPost,
        density = inputs$density, shareOutParm = inputs$shareOutParm
    )

    alpha <- fit@slopes$alpha
    mval <- fit@slopes$meanval
    M <- fit@mktSize
    pricePre <- fit@pricePre
    mcPre <- fit@mcPre

    sCond <- exp(mval + alpha * pricePre)
    sCond <- sCond / sum(sCond)
    marginLevel <- pricePre - mcPre
    profitsCond <- M * marginLevel * sCond
    names(profitsCond) <- inputs$ownerPre

    ## Material failure caught: incorrect firm-level debt aggregation or
    ## default-threshold algebra would violate s0 = 1 - debt / conditional
    ## profit even if the S4 object still constructs successfully.
    s0Focal_manual <- 1 - inputs$debtPre / profitsCond

    expect_equal(as.numeric(fit@pricePre), as.numeric(cf$pricePre), tolerance = 1e-6)
    expect_equal(as.numeric(fit@mcPre), as.numeric(cf$mcPre), tolerance = 1e-6)
    expect_equal(as.numeric(sCond), as.numeric(cf$sCond), tolerance = 1e-6)
    expect_equal(as.numeric(profitsCond), as.numeric(cf$profitsCond), tolerance = 1e-6)
    expect_equal(as.numeric(s0Focal_manual), as.numeric(cf$s0Focal_manual_1_minus_debt_over_profit),
        tolerance = 1e-6
    )

    ## the package's own calcShares(outSideOnly=TRUE) reproduces the SAME
    ## clipped-to-[0,1] internal outside-share fixture value (README caveat:
    ## the closed form and the internal calcShares value are NOT expected to
    ## be numerically identical to each other -- only each to its own
    ## pre-migration capture).
    internal <- calcShares(fit, preMerger = TRUE, outSideOnly = TRUE)
    expect_equal(as.numeric(internal), as.numeric(cf$s0Focal_internal_calcShares_outSideOnly),
        tolerance = 1e-6
    )
})

## ---------------------------------------------------------------------
## 3. Zero-debt directional sanity: DIRECTION only, not exact match.
## ---------------------------------------------------------------------

test_that("zero-debt merger prices move in the captured direction [A: permanent]", {
    skip_if_not(file.exists(sanity_path), "debt_zerodebt_direction_sanity.rds oracle not present")
    sanity <- readRDS(sanity_path)
    inputs <- sanity$inputs

    set.seed(inputs$seed)
    debt_zero_fit <- logit.debt(
        inputs$prices, inputs$shares, inputs$margins, inputs$ownerPre, inputs$ownerPost,
        debtPre = rep(0, length(inputs$prices)), debtPost = rep(0, length(inputs$prices)),
        density = "uniform", shareOutParm = c(1, 1)
    )

    pctChange <- 100 * (debt_zero_fit@pricePost / debt_zero_fit@pricePre - 1)

    ## Material failure caught: a conduct/ownership regression that reverses
    ## the post-merger price effect can pass all constructor and shape checks.
    ## Per README: this is a SIGN/DIRECTION check only. Do not assert exact
    ## numeric equality against the fixture; both this migrated run and the
    ## captured antitrust::logit() comparator must show price INCREASES for
    ## every product post-merger.
    expect_true(all(pctChange > 0), info = "all prices should rise post-merger (logit.debt)")
    expect_true(all(as.numeric(sanity$antitrust_logit$pctChange) > 0),
        info = "all prices should rise post-merger (antitrust::logit fixture)"
    )
    ## same ORDER of magnitude/ranking across products (merging parties 1&2
    ## then rival 3), consistent with the captured fixture's direction.
    expect_equal(order(pctChange), order(as.numeric(sanity$logit_debt$pctChange)))
})
