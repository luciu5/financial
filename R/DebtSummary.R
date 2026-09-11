#' Summary and diagnostic plot methods for `LogitDebt`
#'
#' `summary` and `plotBeta` methods for the `LogitDebt` class, migrated from
#' `MergerDebt/R/Debt-methods.R` (SHA `ea727430`).
#'
#' @name LogitDebt-summary
#' @param object an instance of class `LogitDebt`.
#' @param revenue when `TRUE`, computes revenue shares/quantities. Default
#'   is `FALSE`.
#' @param shares when `TRUE` (default), reports shares. When `FALSE` and
#'   `calcQuantities` is available, reports quantities (not implemented for
#'   `LogitDebt`; falls back to shares with a warning, matching legacy
#'   behavior).
#' @param levels when `TRUE`, report level (not percentage) changes.
#' @param parameters when `TRUE`, print calibrated demand parameters and
#'   Beta shape parameters.
#' @param market when `TRUE`, aggregate results to the market level
#'   (HHI, HHI-debt, Paasche index, leverage, `isProfitable`).
#' @param insideOnly when `TRUE` (default), report conditional (inside-good)
#'   shares.
#' @param digits Number of significant digits to print.
#' @param ... currently unused; retained for generic compatibility.
#' @return A data frame of results, invisibly.
#' @include DebtCounterfactual.R
NULL

#' Plot the calibrated outside-share Beta density
#'
#' @param object A `LogitDebt` object.
#' @param ... Currently unused.
#' @rdname plotBeta
#' @export
setGeneric(
    name = "plotBeta",
    def = function(object, ...) standardGeneric("plotBeta")
)

#' @rdname plotBeta
#' @export
setMethod(
    f = "plotBeta",
    signature = "LogitDebt",
    definition = function(object, ...) {
        shareOutParm <- object@shareOutParm

        p <- seq(0, 1, length = 100)

        plot(p, stats::dbeta(p, shareOutParm[1], shareOutParm[2]),
            ylab = "density",
            type = "l", col = "purple",
            main = paste0(
                "Beta(", round(shareOutParm[1], 2), ",",
                round(shareOutParm[2], 2), ")"
            )
        )
    }
)

#' @rdname LogitDebt-summary
#' @export
setMethod(
    f = "summary",
    signature = "LogitDebt",
    definition = function(object, revenue = FALSE, shares = TRUE, levels = FALSE,
        parameters = FALSE, market = FALSE, insideOnly = TRUE, digits = 2, ...) {
        pricePre <- object@pricePre
        pricePost <- object@pricePost

        priceDelta <- calcPriceDelta(object, levels = levels)

        if (!levels) priceDelta <- priceDelta * 100

        if (!shares && !all(is.na(object@prices))) {
            warning("'shares' equals FALSE but 'calcQuantities' is not defined for 'LogitDebt'. Reporting shares instead of quantities")
            shares <- TRUE
        }

        outPre <- calcShares(object, preMerger = TRUE, revenue = revenue, insideOnly = insideOnly) * 100
        outPost <- calcShares(object, preMerger = FALSE, revenue = revenue, insideOnly = insideOnly) * 100

        sumlabels <- paste("shares", c("Pre", "Post"), sep = "")

        mcDelta <- object@mcDelta

        if (levels) {
            outDelta <- outPost - outPre
        } else {
            outDelta <- (outPost / outPre - 1) * 100
        }

        isParty <- which(object@ownerPost != object@ownerPre)
        partyID <- unique(c(
            object@ownerPre[isParty],
            object@ownerPost[isParty]
        ))
        isParty <- object@ownerPre %in% partyID

        isParty <- factor(as.numeric(isParty), levels = 0:1, labels = c(" ", "*"))

        results <- data.frame(
            isParty,
            Market = object@marketID,
            Product = object@productID,
            ownerPre = object@ownerPre,
            ownerPost = object@ownerPost,
            pricePre = pricePre, pricePost = pricePost,
            priceDelta = priceDelta, outputPre = outPre,
            outputPost = outPost, outputDelta = outDelta
        )

        results <- results[order(-1 * as.numeric(factor(results$Market)), results$outputPre, decreasing = TRUE), ]

        if (sum(abs(mcDelta)) > 0) results <- cbind(results, mcDelta = mcDelta)

        if (market) {
            results$marketID <- object@marketID
            results$ownerPre <- object@ownerPre
            results$ownerPost <- object@ownerPost

            debts <- dplyr::bind_cols(
                ownerPre = object@ownerPre,
                ownerPost = object@ownerPost,
                debtPre = object@debtPre,
                debtPost = object@debtPost
            )
            debts <- dplyr::group_by(debts, ownerPre, ownerPost)
            debts <- dplyr::summarise_all(debts, .funs = sum, na.rm = TRUE)

            if (all(debts$debtPre == debts$debtPost)) debts$debtPost <- NULL

            results <- dplyr::inner_join(results, debts, by = c("ownerPre", "ownerPost"))

            profitable <- results[isParty == "*", ]
            profitable <- dplyr::group_by(profitable, marketID)
            profitable <- dplyr::summarise(profitable,
                profitsPost = sum(outputPost * (pricePost - object@mcPost)),
                profitsPre = sum(outputPre * (pricePre - object@mcPre))
            )
            profitable <- dplyr::mutate(profitable,
                leverage = profitsPre,
                isProfitable = (profitsPost - profitsPre) > 0
            )
            profitable <- dplyr::select(profitable, !dplyr::starts_with("profit"), leverage)
            profitable <- dplyr::ungroup(profitable)

            hhisum <- dplyr::ungroup(results)
            hhisum$isParty <- isParty
            hhisum <- dplyr::group_by(hhisum, ownerPre, marketID, isParty)
            hhisum <- dplyr::summarize(
                hhisum,
                paascheNum = sum(outputPost * pricePost),
                paascheDen = sum(outputPost * pricePre),
                `Pre-Merger HHI` = sum(outputPre),
                `Pre-Merger Debt HHI` = sum(debtPre)
            )
            hhisum <- dplyr::ungroup(hhisum)

            hhisum <- dplyr::group_by(hhisum, marketID)
            hhisum <- dplyr::summarize(
                hhisum,
                `HHI Change` = 2 * prod(`Pre-Merger HHI`[isParty == "*"] / sum(`Pre-Merger HHI`) * 100),
                `HHI Debt Change` = 2 * prod((`Pre-Merger Debt HHI` / sum(`Pre-Merger Debt HHI`))[isParty == "*"] * 100),
                `Pre-Merger Debt HHI` = sum((`Pre-Merger Debt HHI` / sum(`Pre-Merger Debt HHI`) * 100)^2),
                `Pre-Merger HHI` = sum((`Pre-Merger HHI` / sum(`Pre-Merger HHI`) * 100)^2),
                paasche = (sum(paascheNum) / sum(paascheDen) - 1) * 100
            )
            hhisum <- dplyr::mutate(hhisum,
                `Post-Merger HHI` = `Pre-Merger HHI` + `HHI Change`,
                `Post-Merger Debt HHI` = `Pre-Merger Debt HHI` + `HHI Debt Change`
            )
            hhisum <- dplyr::ungroup(hhisum)
            hhisum <- dplyr::relocate(hhisum, paasche, `Pre-Merger HHI`, `Post-Merger HHI`, .after = marketID)

            outAll <- dplyr::inner_join(hhisum, profitable, by = "marketID")
            outAll <- dplyr::ungroup(outAll)
            results <- outAll
        }

        colnames(results)[colnames(results) %in% c("outputPre", "outputPost")] <- sumlabels

        print(results, digits = digits, row.names = FALSE)

        if (!market) {
            cat("\nMerger simulation results under '", class(object), "' demand:\n\n", sep = "")

            cat("\n\tNotes: '*' indicates merging parties' products.\n ")
            if (levels) {
                cat("\tDeltas are level changes.\n")
            } else {
                cat("\tDeltas are percent changes.\n")
            }
            if (revenue) {
                cat("\tOutput is based on revenues.\n")
            } else {
                cat("\tOutput is based on units sold.\n")
            }
        }

        cat("\n\n")

        if (parameters) {
            cat("\nDemand Parameter Estimates:\n\n")
            if (is.list(object@slopes)) {
                print(lapply(object@slopes, round, digits = digits))
            } else {
                print(round(object@slopes, digits))
            }
            cat("\n\n")

            ## Legacy note (preserved, not "fixed"): the legacy code guarded
            ## this block with `.hasSlot(object, "ShareOutParm")` (capital
            ## S), which never matches the actual slot name `shareOutParm`
            ## and so never printed in the legacy package. That guard is a
            ## print-only cosmetic dead branch, not part of the calibrated/
            ## simulated economics covered by the parity oracle, so it is
            ## preserved as-is rather than silently "fixed" here -- fixing
            ## print-only behavior is out of scope for a parity migration.
            if (.hasSlot(object, "ShareOutParm")) {
                cat("\nBeta Shape Parameters:\n\n")
                print(round(object@shareOutParm, digits))
                cat("\n\n")
            }
        }

        return(invisible(results))
    }
)
