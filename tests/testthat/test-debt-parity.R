## Parity regression tests for the migrated debt module.
##
## Rule of use (tests/oracles/README.md): the migrated `financial` code must
## be tested AGAINST THE PRE-MIGRATION FIXTURES captured from legacy
## MergerDebt (SHA ea727430) execution, never against itself.

grid_path <- testthat::test_path("..", "oracles", "debt_goldenmaster_grid.rds")
sanity_path <- testthat::test_path("..", "oracles", "debt_zerodebt_direction_sanity.rds")
closedform_path <- testthat::test_path("..", "oracles", "debt_singlemarket_closedform.rds")

## ---------------------------------------------------------------------
## 1. Golden-master grid: 6 cases crossing debt level x density.
## ---------------------------------------------------------------------

test_that("logit.debt() reproduces the golden-master grid fixture", {
  skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    skip_if_not(file.exists(grid_path), "debt_goldenmaster_grid.rds oracle not present")
    grid <- readRDS(grid_path)

    ## Tight tolerance to start, per the migration brief. The 'zero_uniform'
    ## case is documented (tests/oracles/README.md) to trigger a BBsolve
    ## non-convergence warning in the legacy capture; we expect/allow that
    ## same warning here rather than "fixing" the solver's behavior, and we
    ## widen its tolerance below because non-convergent BBsolve runs are not
    ## guaranteed bit-identical even when the model code is unchanged
    ## (documented, not silently loosened).
    tol_default <- 1e-6
    tol_zero_uniform <- 1e-4

    for (case_name in names(grid)) {
        fixture <- grid[[case_name]]
        inputs <- fixture$inputs

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

        set.seed(inputs$seed)

        tol <- if (identical(case_name, "zero_uniform")) tol_zero_uniform else tol_default

        if (identical(case_name, "zero_uniform")) {
            expect_warning(
                fit <- do.call(logit.debt, args),
                regexp = "BBsolve|converge",
                info = case_name
            )
        } else {
            fit <- do.call(logit.debt, args)
        }

        expect_equal(as.numeric(fit@pricePre), as.numeric(fixture$pricePre),
            tolerance = tol, info = paste(case_name, "pricePre")
        )
        expect_equal(as.numeric(fit@pricePost), as.numeric(fixture$pricePost),
            tolerance = tol, info = paste(case_name, "pricePost")
        )
        expect_equal(as.numeric(fit@shareOutParm), as.numeric(fixture$shareOutParm_calibrated),
            tolerance = tol, info = paste(case_name, "shareOutParm")
        )

        sharesPre <- calcShares(fit, preMerger = TRUE)
        sharesPost <- calcShares(fit, preMerger = FALSE)
        expect_equal(as.numeric(sharesPre), as.numeric(fixture$sharesPre),
            tolerance = tol, info = paste(case_name, "sharesPre")
        )
        expect_equal(as.numeric(sharesPost), as.numeric(fixture$sharesPost),
            tolerance = tol, info = paste(case_name, "sharesPost")
        )
    }
})

## ---------------------------------------------------------------------
## 2. Single-market closed form: s0Focal = 1 - debt/profit.
## ---------------------------------------------------------------------

test_that("single-market s0Focal closed form matches the pre-migration fixture", {
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

test_that("zero-debt logit.debt() agrees in DIRECTION with antitrust::logit()", {
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
