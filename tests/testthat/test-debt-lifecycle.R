## Lifecycle-contract tests for the DebtFit wrapper: exercises debt_fit(),
## simulate(DebtFit), and the ARCHITECTURE-mandated uncertainty-delta outputs
## (Δ default probability / survival mass). These cover the FinancialFit-facing
## entry points, which the legacy-parity tests (test-debt-parity.R) do not.

.mk_debt_args <- function() {
    list(
        prices = c(10, 10, 10), shares = c(0.3, 0.3, 0.4),
        margins = c(0.5, 0.5, 0.5),
        ownerPre = c("1", "2", "3"), ownerPost = c("1", "1", "3"),
        debtPre = c(0.5, 0.5, 0.5), debtPost = c(0.5, 0.5, 0.5),
        mktSize = 1, density = "beta"
    )
}

test_that("debt_fit() builds a valid DebtFit that extends FinancialFit", {
    set.seed(1)
    df <- do.call(debt_fit, .mk_debt_args())
    expect_s4_class(df, "DebtFit")
    expect_true(is(df, "FinancialFit"))
    expect_true(is(df, "StructuralFit"))
    expect_s4_class(df@model, "LogitDebt")           # wraps the legacy object
    expect_true(is.list(df@diagnostics))
})

test_that("simulate(DebtFit) recomputes post-merger prices and emits uncertainty deltas", {
    set.seed(1)
    df <- do.call(debt_fit, .mk_debt_args())
    sim <- simulate(df)
    expect_s4_class(sim, "DebtFit")
    expect_identical(sim@diagnostics$route, "simulate")

    ud <- sim@diagnostics$uncertainty_delta
    expect_true(is.list(ud))
    expect_null(ud$error)
    expect_setequal(ud$firm, c("1", "2", "3"))

    # Default probabilities are valid probabilities where defined.
    dp_pre <- ud$default_prob_pre[c("1", "3")]
    dp_post <- ud$default_prob_post[c("1", "3")]
    expect_true(all(dp_pre >= 0 & dp_pre <= 1))
    expect_true(all(dp_post >= 0 & dp_post <= 1))

    # Economic direction: the merging firm's default probability should FALL
    # (the debt/insurance effect -- pooled profit raises the survival threshold).
    expect_lt(ud$default_prob_post[["1"]], ud$default_prob_pre[["1"]])
    expect_lt(ud$delta_default_prob[["1"]], 0)

    # Firm 2 is folded into owner "1" post-merger, so its post value is NA.
    expect_true(is.na(ud$default_prob_post[["2"]]))
})

test_that("simulate_steps() and respecify() fail loudly on DebtFit (default-reject)", {
    set.seed(1)
    df <- do.call(debt_fit, .mk_debt_args())
    expect_error(simulate_steps(df), regexp = "not implemented|FinancialFit|DebtFit")
    expect_error(respecify(df), regexp = "not implemented|FinancialFit|DebtFit")
})
