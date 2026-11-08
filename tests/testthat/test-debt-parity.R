# Historical fixtures are evidence, not equilibrium targets after numerical
# defects are repaired. A checks below prevent economically invalid legacy
# outputs from being recertified by matching NaNs or an unchanged starting root.
grid_path <- testthat::test_path("..", "oracles", "debt_goldenmaster_grid.rds")
debt_grid_args <- function(inputs) {
    inputs$seed <- NULL
    inputs[!vapply(inputs, is.null, logical(1))]
}

debt_expect_foc <- function(fit, pre) {
    p <- if (pre) fit@pricePre else fit@pricePost
    c <- if (pre) fit@mcPre else fit@mcPost
    own <- if (pre) fit@ownerPre else fit@ownerPost
    m <- length(unique(fit@marketID))
    s <- matrix(calcShares(fit, pre), m)
    own <- matrix(own, m)[1, ]
    firm <- s
    for (j in seq_len(ncol(s))) firm[, j] <- rowSums(s[, own == own[j], drop = FALSE])
    residual <- matrix(p - c, m) + 1 / (fit@slopes$alpha * (1 - firm))
    expect_true(all(is.finite(s)))
    expect_true(all(s >= 0 & s <= 1))
    expect_lt(max(abs(residual)), 1e-6)
}

test_that("A: repaired uniform grid obeys finite ownership FOCs rather than trap roots", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    grid <- readRDS(grid_path)
    for (nm in c("zero_uniform", "low_uniform", "zero_beta", "low_beta")) {
        fit <- do.call(logit.debt, debt_grid_args(grid[[nm]]$inputs))
        debt_expect_foc(fit, TRUE)
        debt_expect_foc(fit, FALSE)
        inside <- matrix(calcShares(fit, TRUE, insideOnly = TRUE), 2)
        expect_equal(rowSums(inside), rep(1, 2), tolerance = 1e-6)
    }
})

test_that("C: impossible high-debt conditioning is rejected instead of matching legacy NaNs", {
    # The original high-beta capture falsely passed because both sides were
    # NaN. These debts exceed maximum conditional profit at the observations.
    grid <- readRDS(grid_path)
    for (nm in c("high_uniform", "high_beta")) {
        expect_error(do.call(logit.debt, debt_grid_args(grid[[nm]]$inputs)),
            class = "financial_zero_survival")
    }
})


test_that("A: multimarket Beta calibration minimizes the correctly normalized moment criterion", {
    skip_if(Sys.getenv("FINANCIAL_EXTENDED_TESTS") != "1", "extended validation")
    fit <- do.call(logit.debt, debt_grid_args(readRDS(grid_path)$low_beta$inputs))
    observed_margin <- matrix(fit@margins * fit@prices, 2)
    observed_profit <- observed_margin * matrix(fit@shares, 2)
    debt <- as.numeric(tapply(fit@debtPre, fit@ownerPre, sum, na.rm = TRUE))
    criterion <- function(mu) {
        candidate <- fit
        precision <- sum(fit@shareOutParm)
        candidate@shareOutParm <- precision * c(mu, 1 - mu)
        candidate@pricePre <- fit@prices
        candidate@mcPre <- fit@prices - as.numeric(observed_margin)
        residual <- (as.numeric(observed_margin) -
            calcMargins(candidate, TRUE, level = TRUE)) / fit@prices
        # Independent observed-profit solvency boundary evaluated at E[U]
        # in the other market, then population moments of those firm cutoffs.
        cutoff <- (colSums(observed_profit) - debt -
            observed_profit[2, ] * mu) / observed_profit[1, ]
        cutoff <- pmax(0, pmin(1, cutoff))
        sum(c(residual, mean(cutoff) - mu,
            var(cutoff) - mu * (1 - mu) / (precision + 1))^2)
    }
    mu <- fit@shareOutParm[1] / sum(fit@shareOutParm)
    center <- criterion(mu)
    expect_lte(center, criterion(mu - .01) + 1e-6)
    expect_lte(center, criterion(mu + .01) + 1e-6)
})
