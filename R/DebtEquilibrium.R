#' Equilibrium methods for the `LogitDebt` class
#'
#' `calcShares`, `calcSlopes`, `calcPrices`, `calcMC`, `calcMargins` methods
#' for the `LogitDebt` class, migrated from
#' `MergerDebt/R/Debt-methods.R` (SHA `ea727430`). Numerical repairs evaluate
#' the inherited default-conditioned equations on their actual support and
#' reject undefined conditioning or unverified equilibria.
#'
#' Per `design/ARCHITECTURE.md`: limited liability lives in the demand/share
#' layer via the `s0Focal` default-threshold (profit = debt boundary),
#' reshaping conditional logit shares through partial-Beta integrals. This
#' inherited economic specification remains unchanged; zero-survival regimes
#' have no defined conditional shares and produce a structured error.
#'
#' The legacy per-object `hFocalIntegral`/`gFocalIntegral` closures are
#' replaced by calls to the shared
#' `beta_h_focal_integral()`/`beta_g_focal_integral()` helpers from
#' `shared-beta-moments.R`, with algebraic moment prefactors that avoid
#' underflow in ratios of Beta functions.
#'
#' @name LogitDebt-methods
#' @param object an instance of class `LogitDebt`.
#' @param preMerger when `TRUE`, computes equilibrium prices/shares/margins
#'   under the pre-merger regime. When `FALSE`, calculates the post-merger
#'   equivalent. Default is `TRUE`.
#' @param revenue when `TRUE`, computes revenue shares. When `FALSE`,
#'   calculates quantity shares. Default is `FALSE`.
#' @param insideOnly when `TRUE`, returns the conditional (inside-good) share
#'   only, without reshaping by the outside-share default threshold.
#' @param outSideOnly when `TRUE`, returns only the calibrated/simulated
#'   outside-share default threshold (`s0Focal`/`shareOutOther`) rather than
#'   product shares.
#' @param level when `TRUE`, computes margins in dollars. When `FALSE`,
#'   calculates margins as a proportion of prices. Default is `FALSE`.
#' @param isMax if `TRUE`, runs a numerical check (via `numDeriv::genD`) of
#'   whether the computed equilibrium price vector is a local profit
#'   maximum. Default is `FALSE`.
#' @param ... additional arguments passed through to [BB::BBsolve()].
#' @return `calcSlopes` returns a `LogitDebt` object containing estimated
#'   slopes. `calcShares`/`calcPrices`/`calcMC`/`calcMargins` return the
#'   corresponding named numeric vector (or, for `calcShares(outSideOnly =
#'   TRUE)`, a market-by-firm matrix).
#' @include DebtClasses.R
NULL

# Debt conditioning is defined only when there is positive solvent mass.
# Keep these conditions distinguishable for callers without changing the
# banking module's shared numerical primitives or inventing default outcomes.
.debt_stop <- function(kind, message) {
    stop(structure(list(message = message, call = NULL),
        class = c(paste0("financial_", kind), "error", "condition")))
}

# Integrate E[(1-U_m) 1{profit >= debt}] and its U_m-weighted counterpart.
# Integrating one outside share analytically avoids a discontinuous indicator;
# integrating the remaining coordinates in probability space resolves highly
# concentrated Beta densities. Every market retains its own moments.
.debt_survival_moments <- function(profit, debt, shape, focal = 1L, control = list()) {
    n <- length(profit)
    if (any(!is.finite(profit)) || !is.finite(debt) || debt < 0 ||
        length(shape) != 2L || any(!is.finite(shape)) || any(shape <= 0)) {
        .debt_stop("equilibrium_failure", "invalid profit, debt or Beta parameters")
    }
    if (length(focal) != 1L || !is.finite(focal) || focal != as.integer(focal) ||
        focal < 1 || focal > n) {
        .debt_stop("equilibrium_failure", "focal must identify a market")
    }
    total <- sum(shape)
    full_g <- shape[2] / total
    full_h <- (shape[1] / total) * (shape[2] / (total + 1))
    # No default occurs with zero debt and nonnegative conditional profits.
    if (debt == 0 && all(profit >= 0)) {
        return(list(g = rep(full_g, n), h = rep(full_h, n)))
    }
    if (sum(pmax(profit, 0)) <= debt) {
        .debt_stop("zero_survival", "default conditioning has zero solvent probability")
    }
    axis <- if (profit[focal] > 0) focal else which.max(profit)
    other <- setdiff(seq_len(n), axis)
    integrand <- function(probability) {
        weight <- 1
        if (all(profit >= 0)) {
            # Map the integration cube into the solvent support. A generic
            # cubature rule can otherwise miss an entire small simplex and
            # return a falsely exact zero. The conditional probability limits
            # supply the Jacobian of this triangular transformation.
            remaining <- sum(profit) - debt
            u <- numeric(length(other))
            for (j in seq_along(other)) {
                limit <- if (profit[other[j]] == 0) 1 else
                    max(0, min(1, remaining / profit[other[j]]))
                probability_limit <- stats::pbeta(limit, shape[1], shape[2])
                weight <- weight * probability_limit
                u[j] <- stats::qbeta(probability[j] * probability_limit, shape[1], shape[2])
                remaining <- remaining - profit[other[j]] * u[j]
            }
            cutoff <- remaining / profit[axis]
        } else {
            u <- stats::qbeta(probability, shape[1], shape[2])
            cutoff <- (sum(profit) - debt - sum(u * profit[other])) / profit[axis]
        }
        g <- h <- numeric(n)
        g[axis] <- beta_g_focal_integral(cutoff, shape[1], shape[2])
        h[axis] <- beta_h_focal_integral(cutoff, shape[1], shape[2])
        survival <- stats::pbeta(cutoff, shape[1], shape[2])
        g[other] <- (1 - u) * survival
        h[other] <- u * g[other]
        c(g, h) * weight
    }
    if (n == 1L) {
        value <- integrand(numeric())
    } else {
        rel <- if (is.null(control$relTol)) 1e-9 else min(control$relTol, 1e-8)
        abs <- if (is.null(control$absTol)) 1e-11 else min(control$absTol, 1e-10)
        if (!is.finite(rel) || rel <= 0 || !is.finite(abs) || abs < 0) {
            .debt_stop("integration_failure", "invalid integration tolerances")
        }
        if (n == 2L) {
            # Split where the analytic cutoff crosses zero and one. Without
            # support-aware splitting an adaptive rule can see only zeros.
            boundaries <- c(0, .01, .1, .5, .9, .99, 1)
            if (profit[other] != 0) {
                crossing <- (sum(profit) - debt - c(0, profit[axis])) / profit[other]
                crossing <- crossing[crossing > 0 & crossing < 1]
                boundaries <- sort(unique(c(boundaries,
                    stats::pbeta(crossing, shape[1], shape[2]))))
            }
            value <- numeric(2L * n)
            for (k in seq_len(length(boundaries) - 1L)) {
                if (boundaries[k + 1L] <= boundaries[k]) next
                for (j in seq_along(value)) {
                    result <- tryCatch(stats::integrate(function(z) {
                        probability_limit <- 1
                        if (all(profit >= 0) && profit[other] > 0) {
                            limit <- min(1, (sum(profit) - debt) / profit[other])
                            probability_limit <- stats::pbeta(limit, shape[1], shape[2])
                        }
                        u <- stats::qbeta(z * probability_limit, shape[1], shape[2])
                        cutoff <- (sum(profit) - debt - profit[other] * u) / profit[axis]
                        market <- if (j <= n) j else j - n
                        if (market == axis) {
                            moment <- if (j <= n) beta_g_focal_integral(cutoff, shape[1], shape[2]) else
                                beta_h_focal_integral(cutoff, shape[1], shape[2])
                        } else {
                            moment <- (1 - u) * stats::pbeta(cutoff, shape[1], shape[2])
                            if (j > n) moment <- moment * u
                        }
                        moment * probability_limit
                    }, boundaries[k], boundaries[k + 1L], rel.tol = rel,
                        abs.tol = abs, subdivisions = 1000L, stop.on.error = FALSE),
                        error = function(e) .debt_stop("integration_failure", conditionMessage(e)))
                    if (!(result$message == "OK" || grepl("roundoff error", result$message, fixed = TRUE)) ||
                        !is.finite(result$value) ||
                        !is.finite(result$abs.error) ||
                        result$abs.error > max(abs, rel * abs(result$value))) {
                        .debt_stop("integration_failure", paste0("outside-share quadrature failed its error bound: ",
                            result$message, "; error = ", result$abs.error))
                    }
                    value[j] <- value[j] + result$value
                }
            }
        } else {
            # fDim rows and one column per vectorized cubature sample.
            result <- tryCatch(cubature::cubintegrate(function(x) {
                x <- matrix(x, nrow = n - 1L)
                vapply(seq_len(ncol(x)), function(i) integrand(x[, i]), numeric(2L * n))
            }, lower = rep(0, n - 1L), upper = rep(1, n - 1L), fDim = 2L * n,
                nVec = if (is.null(control$nVec)) 1024L else control$nVec,
                method = if (is.null(control$method)) "hcubature" else control$method,
                relTol = rel, absTol = abs,
                maxEval = if (is.null(control$maxEval)) 1e6 else control$maxEval),
                error = function(e) .debt_stop("integration_failure", conditionMessage(e)))
            value <- result$integral
            if (length(value) != 2L * n || any(!is.finite(value)) ||
                any(!is.finite(result$error)) ||
                any(result$error > pmax(abs, rel * abs(value))) ||
                (!is.null(result$returnCode) && result$returnCode != 0)) {
                .debt_stop("integration_failure", "outside-share cubature failed its error bound")
            }
        }
    }
    g <- value[seq_len(n)]
    h <- value[n + seq_len(n)]
    if (any(!is.finite(value)) || any(g <= 0) || any(h < 0) || any(h > g)) {
        .debt_stop("integration_failure", "positive solvent mass could not be resolved numerically")
    }
    list(g = g, h = h)
}

#' @rdname LogitDebt-methods
#' @export
setMethod(
    f = "calcSlopes",
    signature = "LogitDebt",
    definition = function(object) {
        if (any(!is.finite(object@margins))) {
            .debt_stop("equilibrium_failure", paste0(
                "debt calibration requires complete finite 'margins'; product positions: ",
                paste(which(!is.finite(object@margins)), collapse = ", ")))
        }
        nMarkets <- length(unique(object@marketID))

        ownerPre <- object@ownerPre
        ownerPreMat <- matrix(ownerPre, nrow = nMarkets)
        ownerPreMat <- ownerPreMat[1, ]

        shares <- matrix(object@shares, nrow = nMarkets)
        margins <- matrix(object@margins, nrow = nMarkets)
        prices <- matrix(object@prices, nrow = nMarkets)
        idx <- object@normIndex
        focal <- object@focal
        M <- object@mktSize

        debt <- tapply(object@debtPre, ownerPre, sum, na.rm = TRUE)
        debt <- stats::setNames(as.numeric(debt), names(debt))

        shareOutParm <- object@shareOutParm
        parmsStart <- object@parmsStart

        margins <- margins * prices

        profitsCond <- M * margins * shares

        ## aggregate profits to the firm level
        profitsCond <- t(rowsum(t(profitsCond), group = ownerPreMat, na.rm = TRUE))

        s0Focal <- function(i, sOut) {
            if (nMarkets == 1) {
                res <- 1 - debt[i] / profitsCond[, i]
                return(res)
            }

            res <- (sum(profitsCond[, i, drop = FALSE]) - debt[i]) / profitsCond[focal, i, drop = TRUE] -
                sum(as.vector(sOut) * profitsCond[-focal, i, drop = FALSE]) / profitsCond[focal, i, drop = TRUE]

            return(res)
        }

        thisobj <- object

        thisobj@pricePre <- as.numeric(prices)
        thisobj@mcPre <- -1 * as.numeric(margins - prices)

        minD <- function(theta) {
            alpha <- theta[1:nMarkets]
            if (all(is.na(shareOutParm))) {
                outParm <- theta[-(1:nMarkets)]
            } else {
                outParm <- shareOutParm
            }

            thisobj@slopes <- list(
                alpha = alpha,
                meanval = as.numeric(log(shares) - log(shares[, idx]) - alpha * (prices - prices[, idx]))
            )

            thisobj@shareOutParm <- outParm

            marginsCand <- calcMargins(thisobj, preMerger = TRUE, level = TRUE)

            sOutMean <- sapply(ownerPreMat, s0Focal, sOut = outParm[1] / sum(outParm))
            sOutMean[sOutMean < 0] <- 0
            sOutMean[sOutMean > 1] <- 1

            m2 <- m3 <- NA
            m1 <- (as.numeric(margins) - marginsCand) / prices
            if (all(is.na(shareOutParm))) {
                m2 <- mean(sOutMean, na.rm = TRUE) - outParm[1] / sum(outParm) # mean of Beta
                m3 <- stats::var(sOutMean, na.rm = TRUE) - prod(outParm) / (sum(outParm)^2 * (sum(outParm) + 1)) # variance of Beta
            }

            residual <- if (all(is.na(shareOutParm))) c(m1, m2, m3) else m1
            residual <- residual[c(!is.na(as.numeric(margins)), rep(TRUE, length(residual) - length(margins)))]
            if (any(!is.finite(residual))) {
                .debt_stop("equilibrium_failure", "non-finite demand calibration residual")
            }
            measure <- sum((residual * 100)^2)

            return(measure)
        }

        ## Constrain optimizer to look alpha < 0, 1e5 >= Beta Parameters >= 1e-5
        lowerB <- c(rep(-1e3, nMarkets), 1e-5, 1e-5)
        upperB <- c(rep(-1e-10, nMarkets), 1e5, 1e5)

        if (all(!is.na(shareOutParm))) {
            lowerB <- lowerB[1:nMarkets]
            upperB <- upperB[1:nMarkets]
            parmsStart <- parmsStart[1:nMarkets]
        }

        minTheta <- stats::optim(parmsStart, minD,
            method = "L-BFGS-B",
            lower = lowerB, upper = upperB,
            control = object@control.slopes
        )

        if (minTheta$convergence != 0 && all(is.finite(minTheta$par))) {
            # Support boundaries can make finite-difference L-BFGS line
            # searches fail. Re-optimize the same criterion without gradients
            # in positive magnitudes; retain exactly the same parameter bounds.
            sign <- c(rep(-1, nMarkets), rep(1, length(minTheta$par) - nMarkets))
            objective <- function(log_parameter) {
                theta <- sign * exp(log_parameter)
                if (any(!is.finite(theta)) || any(theta < lowerB | theta > upperB)) return(1e30)
                minD(theta)
            }
            refined <- stats::optim(log(abs(minTheta$par)), objective,
                method = "Nelder-Mead", control = list(maxit = 5000, reltol = 1e-10))
            minTheta <- list(par = sign * exp(refined$par), value = refined$value,
                convergence = refined$convergence)
        }
        if (minTheta$convergence != 0 || any(!is.finite(minTheta$par)) ||
            !is.finite(minTheta$value)) {
            .debt_stop("equilibrium_failure", "demand calibration did not converge to finite parameters")
        }
        minAlpha <- minTheta$par[1:nMarkets]
        minOutParm <- minTheta$par[-(1:nMarkets)]
        meanval <- log(shares) - log(shares[, idx]) - minAlpha * (prices - prices[, idx])
        meanval <- as.numeric(meanval)
        names(meanval) <- object@labels

        object@slopes <- list(alpha = minAlpha, meanval = meanval)
        if (all(is.na(shareOutParm))) {
            object@shareOutParm <- minOutParm

            if (any(minOutParm == upperB[-(1:nMarkets)] | minOutParm == lowerB[-(1:nMarkets)])) {
                warning("'shareOutParm' estimates are at either upper or lower bound")
            }
            if (any(minOutParm == parmsStart[-(1:nMarkets)])) {
                warning("'shareOutParm' estimates are equal to starting values")
            }
        }

        return(object)
    }
)

#' @rdname LogitDebt-methods
#' @export
setMethod(
    f = "calcShares",
    signature = "LogitDebt",
    definition = function(object, preMerger = TRUE, revenue = FALSE, insideOnly = FALSE, outSideOnly = FALSE) {
        if (preMerger) {
            debt <- object@debtPre
            prices <- object@pricePre
            owner <- factor(object@ownerPre, levels = unique(object@ownerPre))
            mc <- object@mcPre
        } else {
            debt <- object@debtPost
            prices <- object@pricePost
            owner <- factor(object@ownerPost, levels = unique(object@ownerPost))
            mc <- object@mcPost
        }

        firmVec <- levels(owner)
        nFirms <- nlevels(owner)
        nMarkets <- length(unique(object@marketID))
        slopes <- object@slopes$alpha
        mval <- object@slopes$meanval
        M <- object@mktSize
        shareOutParm <- object@shareOutParm
        focal <- object@focal
        control.cub <- object@control.cub
        density <- object@density
        if (all(!is.na(shareOutParm)) &&
            shareOutParm[1] == 1 &&
            shareOutParm[1] == shareOutParm[2]) {
            density <- "uniform"
        }

        debt <- tapply(debt, owner, sum, na.rm = TRUE)
        debt <- stats::setNames(as.numeric(debt), names(debt))

        if (nMarkets > 1) {
            prices <- matrix(prices, nrow = nMarkets)
            mc <- matrix(mc, nrow = nMarkets)
            mval <- matrix(mval, nrow = nMarkets)
            owner <- matrix(as.character(owner), nrow = nMarkets)[1, ]
        }

        margin <- prices - mc
        eta <- matrix(mval + slopes * prices, nrow = nMarkets)
        if (any(!is.finite(eta))) .debt_stop("equilibrium_failure", "non-finite conditional utility")
        eta <- eta - apply(eta, 1, max)
        sCond <- exp(eta)
        sCond <- sCond / rowSums(sCond)
        # Conditional demand has no default conditioning or integration.
        if (insideOnly && !outSideOnly) {
            sharesCand <- as.numeric(sCond)
            names(sharesCand) <- object@labels
            return(sharesCand)
        }
        profitsCond <- M * matrix(margin, nrow = nMarkets) * sCond
        profitsCond <- t(rowsum(t(profitsCond), group = owner, reorder = TRUE))
        SharesOutInt <- vapply(firmVec, function(firm) {
            moments <- .debt_survival_moments(profitsCond[, firm], debt[firm],
                shareOutParm, focal, control.cub)
            moments$h / moments$g
        }, numeric(nMarkets))
        SharesOutInt <- matrix(SharesOutInt, nrow = nMarkets,
            dimnames = list(seq_len(nMarkets), firmVec))

        sharesCand <- sCond
        if (outSideOnly) {
            return(SharesOutInt)
        }
        if (!insideOnly) sharesCand <- sharesCand * (1 - SharesOutInt[, owner])

        if (revenue) {
            sharesCand <- prices * sharesCand / sum(prices * sharesCand, object@priceOutside * (1 - sum(sharesCand, na.rm = TRUE)), na.rm = TRUE)
        }

        sharesCand <- as.numeric(sharesCand)
        names(sharesCand) <- object@labels

        return(sharesCand)
    }
)

#' @rdname LogitDebt-methods
#' @export
setMethod(
    f = "calcPrices",
    signature = "LogitDebt",
    definition = function(object, preMerger = TRUE, isMax = FALSE, ...) {
        priceStart <- object@priceStart

        if (preMerger) {
            owner <- object@ownerPre
            mc <- object@mcPre
        } else {
            owner <- object@ownerPost
            mc <- object@mcPost
        }

        thisObj <- object

        ## Define system of FOC as a function of prices
        FOC <- function(priceCand) {
            if (preMerger) {
                thisObj@pricePre <- priceCand
            } else {
                thisObj@pricePost <- priceCand
            }

            thisMargin <- calcMargins(thisObj, preMerger = preMerger, level = TRUE)

            thisFOC <- priceCand - mc - thisMargin

            return(thisFOC)
        }

        # Validate the initial economic state before invoking a solver, which
        # otherwise can interpret missing firm shares as a zero residual.
        initial <- FOC(priceStart)
        if (any(!is.finite(initial))) {
            .debt_stop("equilibrium_failure", "non-finite initial price residual")
        }
        ## Find price changes that set FOCs equal to 0
        minResult <- BB::BBsolve(priceStart, FOC, quiet = TRUE, control = object@control.equ, ...)

        residual <- FOC(minResult$par)
        tol <- object@control.equ$tol
        if (is.null(tol)) tol <- 1e-7
        tol <- min(tol, 1e-7)
        if (all(is.finite(residual)) && max(abs(residual)) > tol) {
            refined <- tryCatch(nleqslv::nleqslv(minResult$par, FOC,
                method = "Newton", control = list(ftol = tol * .1, maxit = 500)),
                error = function(e) NULL)
            if (!is.null(refined) && all(is.finite(refined$fvec)) &&
                max(abs(refined$fvec)) < max(abs(residual))) {
                minResult$par <- refined$x
                residual <- FOC(refined$x)
                minResult$convergence <- if (refined$termcd == 1L) 0L else 1L
            }
        }
        if (minResult$convergence != 0 || any(!is.finite(minResult$par)) ||
            any(!is.finite(residual)) || max(abs(residual)) > tol) {
            .debt_stop("equilibrium_failure", paste0(
                "price solver did not verify a finite equilibrium: ", minResult$message,
                "; residual = ", max(abs(residual))))
        }

        if (isMax) {
            ## Legacy defect fixed: the legacy code called an unqualified
            ## `genD()`, relying on `numDeriv` being attached rather than
            ## imported -- this broke the isMax path under NAMESPACE-strict
            ## loading. Qualify explicitly per design/ARCHITECTURE.md's
            ## "Known legacy defects to fix during migration".
            hess <- numDeriv::genD(FOC, minResult$par) # compute the numerical approximation of the FOC hessian at optimum
            hess <- hess$D[, 1:hess$p]
            hess <- hess * (owner > 0) # 0 terms not under the control of a common owner

            state <- ifelse(preMerger, "Pre-merger", "Post-merger")

            if (any(eigen(hess)$values > 0)) {
                warning("Hessian of first-order conditions is not positive definite. ", state, " price vector may not maximize profits. Consider rerunning 'calcPrices' using different starting values")
            }
        }

        priceEst <- minResult$par
        names(priceEst) <- object@labels

        return(priceEst)
    }
)

#' @rdname LogitDebt-methods
#' @export
setMethod(
    f = "calcMC",
    signature = "LogitDebt",
    definition = function(object, preMerger = TRUE) {
        if (preMerger) {
            object@pricePre <- object@prices

            startMC <- object@prices * (1 - object@margins)
            startMC[is.na(startMC)] <- max(startMC, na.rm = TRUE)

            minD <- function(c) {
                object@mcPre <- c
                thisMargin <- tryCatch(calcMargins(object, preMerger = TRUE, level = TRUE),
                    financial_zero_survival = function(e) NULL)
                # A trial cost outside solvent support is an infeasible
                # optimizer candidate. The accepted result is re-evaluated
                # below without this penalty or any missing-value removal.
                if (is.null(thisMargin)) return(1e12 + sum((c - startMC)^2))
                measure <- object@pricePre - c - thisMargin
                measure <- (measure / object@pricePre)^2
                return(sum(measure))
            }

            lowerB <- rep(0, length(startMC))
            upperB <- object@prices

            minMC <- stats::optim(startMC, minD, method = "L-BFGS-B", lower = lowerB, upper = upperB,
                control = list(factr = 1e3, pgtol = 1e-10, maxit = 1000))

            mc <- minMC$par
            object@mcPre <- mc
            residual <- object@pricePre - mc - calcMargins(object, TRUE, level = TRUE)
            if (minMC$convergence != 0 || any(!is.finite(residual)) ||
                max(abs(residual / object@pricePre)) > 1e-6) {
                .debt_stop("equilibrium_failure", paste0("marginal-cost calibration did not verify a finite equilibrium; residual = ",
                    max(abs(residual / object@pricePre))))
            }
        }

        if (!preMerger) {
            mc <- object@mcPre
            mc <- mc * (1 + object@mcDelta)
        }

        names(mc) <- object@labels

        return(mc)
    }
)

#' @rdname LogitDebt-methods
#' @export
setMethod(
    f = "calcMargins",
    signature = "LogitDebt",
    definition = function(object, preMerger = TRUE, level = FALSE) {
        nMarkets <- length(unique(object@marketID))
        alpha <- object@slopes$alpha

        if (preMerger) {
            prices <- object@pricePre
            owner <- object@ownerPre
        } else {
            prices <- object@pricePost
            owner <- object@ownerPost
        }

        shares <- calcShares(object, preMerger = preMerger, revenue = FALSE)

        if (nMarkets > 1) {
            shares <- matrix(shares, nMarkets)
            prices <- matrix(prices, nMarkets)
            owner <- matrix(owner, nMarkets)[1, ]

            shares <- t(rowsum(t(shares), group = owner))
            shares <- shares[, owner]
        } else {
            shares <- tapply(shares, owner, sum)
            shares <- shares[owner]
        }

        margins <- -1 / (alpha * (1 - shares))

        if (!level) {
            margins <- margins / prices
        }

        margins <- as.numeric(margins)

        names(margins) <- object@labels

        return(margins)
    }
)
