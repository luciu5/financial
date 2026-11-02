#' A shared financial-structural-fit contract
#'
#' `FinancialFit` is the common virtual base for the two disjoint economic
#' families in this package (`DebtFit` and `BankFit`). It contributes no
#' new representation of its own: the financial structural state -- the
#' normalized spec, fitted model, recovered/supplied parameters, observed
#' pre-merger inputs, and diagnostics -- is inherited unchanged from
#' [antitrust::StructuralFit-class]. `FinancialFit` exists purely to give the two
#' families a common dispatch point for the extensible lifecycle generics
#' (`simulate`, `simulate_steps`, `respecify`) so that `antitrust`'s
#' `CounterfactualPath` resume machinery can dispatch on either family
#' polymorphically, and so that unsupported lifecycle operations fail
#' loudly and uniformly rather than silently falling through to unrelated
#' economics.
#'
#' Concrete subclasses (`DebtFit`, `BankFit`) own all economics and must
#' supply their own `simulate`/`simulate_steps`/`respecify` methods. The
#' methods defined here on `FinancialFit` itself are default-reject stubs:
#' they exist only so that calling a lifecycle generic on a `FinancialFit`
#' object that has not (yet, or ever) implemented that operation produces a
#' clear, immediate error instead of dispatching to an unrelated ancestor
#' method or failing with a cryptic "unable to find an inherited method"
#' condition.
#'
#' @section Extends:
#' `FinancialFit` extends [antitrust::StructuralFit-class], directly.
#'
#' @seealso [antitrust::StructuralFit-class]
#' @import antitrust
#' @importClassesFrom antitrust StructuralFit
#' @importFrom antitrust simulate simulate_steps respecify
#' @include financial-package.R
#' @exportClass FinancialFit
setClass(
    Class = "FinancialFit",
    contains = c("StructuralFit", "VIRTUAL")
)

#' @param object A `FinancialFit`.
#' @param ... Additional arguments (unused by the default-reject method).
#' @rdname FinancialFit-class
#' @export
setMethod("simulate", "FinancialFit", function(object, ...) {
    stop(
        "simulate() is not implemented for this FinancialFit subclass (",
        class(object), "). Concrete subclasses (e.g. DebtFit, BankFit) ",
        "must supply their own simulate() method.",
        call. = FALSE
    )
})

#' @param object A `FinancialFit`.
#' @param last_result The legacy S4 result the path last landed on.
#' @param steps The ordered `CounterfactualStep` objects to solve next.
#' @param ... Additional family-specific resume state.
#' @rdname FinancialFit-class
#' @export
setMethod("simulate_steps", "FinancialFit", function(object, last_result, steps, ...) {
    stop(
        "simulate_steps() is not implemented for this FinancialFit subclass (",
        class(object), "). Concrete subclasses (e.g. DebtFit, BankFit) ",
        "must supply their own simulate_steps() method.",
        call. = FALSE
    )
})

#' @param object A `FinancialFit`.
#' @param ... Additional arguments (unused by the default-reject method).
#' @rdname FinancialFit-class
#' @export
setMethod("respecify", "FinancialFit", function(object, ...) {
    stop(
        "respecify() is not implemented for this FinancialFit subclass (",
        class(object), "). Concrete subclasses (e.g. DebtFit, BankFit) ",
        "must supply their own respecify() method.",
        call. = FALSE
    )
})
