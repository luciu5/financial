#' Equilibrium methods for the `LogitDebt` class
#'
#' `calcShares`, `calcSlopes`, `calcPrices`, `calcMC`, `calcMargins` methods
#' for the `LogitDebt` class, migrated verbatim (parity migration -- the
#' economics are preserved exactly, not redesigned) from
#' `MergerDebt/R/Debt-methods.R` (SHA `ea727430`).
#'
#' Per `design/ARCHITECTURE.md`: limited liability lives in the demand/share
#' layer via the `s0Focal` default-threshold (profit = debt boundary),
#' reshaping conditional logit shares through partial-Beta integrals. This
#' is flagged for red-team economic review and is NOT rewritten here.
#'
#' The legacy per-object `hFocalIntegral`/`gFocalIntegral` closures are
#' replaced by calls to the shared, oracle-validated
#' `beta_h_focal_integral()`/`beta_g_focal_integral()` helpers from
#' `shared-beta-moments.R` (verified algebraically identical to the legacy
#' closures -- same `beta(...)/beta(...) * pbeta(...)` expressions).
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

#' @rdname LogitDebt-methods
#' @export
setMethod(
    f = "calcSlopes",
    signature = "LogitDebt",
    definition = function(object) {
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

            sOutMean <- sapply(ownerPreMat, s0Focal, sOut = outParm[1] / sum(outParm[1]))
            sOutMean[sOutMean < 0] <- 0
            sOutMean[sOutMean > 1] <- 1

            m2 <- m3 <- NA
            m1 <- (as.numeric(margins) - marginsCand) / prices
            if (all(is.na(shareOutParm))) {
                m2 <- mean(sOutMean, na.rm = TRUE) - outParm[1] / sum(outParm) # mean of Beta
                m3 <- stats::var(sOutMean, na.rm = TRUE) - prod(outParm) / (sum(outParm)^2 * (sum(outParm) + 1)) # variance of Beta
            }

            measure <- sum((c(m1, m2, m3) * 100)^2, na.rm = TRUE)

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

        lowerB <- rep(0, nMarkets - 1)
        upperB <- rep(1, nMarkets - 1)

        bDens <- function(u, ...) {
            stats::dbeta(u, shape1 = shareOutParm[1], shape2 = shareOutParm[2], ...)
        }
        bProb <- function(u, ...) {
            stats::pbeta(u, shape1 = shareOutParm[1], lower.tail = TRUE, shape2 = shareOutParm[2], ...)
        }

        integrateDerivOtherFun <- function(u, f, pre = preMerger) {
            s0 <- s0Focal(u, f, pre)
            u * (1 - u) * bProb(s0)
        }
        integrateInsideOtherFun <- function(u, f, pre = preMerger) {
            s0 <- s0Focal(u, f, pre)
            (1 - u) * bProb(s0)
        }

        s0Focal <- function(sOut = rep(NA, nMarkets), i, pre = preMerger, n = nMarkets, l = focal) {
            ## allow firm index i to be a vector of length > 1.
            if (n == 1) {
                res <- 1 - debt[i] / profitsCond[, i]
                return(res)
            }

            res <- (sum(profitsCond[, i, drop = FALSE]) - debt[i]) / profitsCond[l, i, drop = TRUE] -
                sum(as.vector(sOut) * profitsCond[-l, i, drop = FALSE]) / profitsCond[l, i, drop = TRUE]

            return(res)
        }

        if (nMarkets > 1) theseStates <- expand.grid(lapply(1:(nMarkets - 1), function(x) 1:0))

        margin <- prices - mc

        sCond <- exp(mval + slopes * prices)
        if (nMarkets > 1) {
            sCond <- sCond / rowSums(sCond)
        } else {
            sCond <- sCond / sum(sCond)
            sCond <- t(as.matrix(sCond))
        }

        profitsCond <- M * margin * sCond

        ## aggregate profits to the firm level
        profitsCond <- t(rowsum(t(profitsCond), group = owner, na.rm = TRUE))

        if (nMarkets == 1) {
            hFun <- sapply(firmVec, function(q) {
                s0_focal <- s0Focal(NA, q)
                beta_h_focal_integral(s0_focal, shape1 = shareOutParm[1], shape2 = shareOutParm[2])
            })
            gFun <- sapply(firmVec, function(q) {
                s0_focal <- s0Focal(NA, q)
                beta_g_focal_integral(s0_focal, shape1 = shareOutParm[1], shape2 = shareOutParm[2])
            })
        } else if (nMarkets >= 2 && density == "uniform") {
            hFun <- sapply(firmVec, function(q, pre = preMerger) {
                s0_focal <- apply(theseStates, 1, s0Focal, i = q)

                betaStates <- 6 / factorial(nMarkets + 1) * s0_focal^(nMarkets + 1)
                betaStates <- beta(2, 2) * (betaStates - 12 / factorial(nMarkets + 2) * s0_focal^(nMarkets + 2))

                st <- ncol(theseStates)
                thisTheseStates <- theseStates
                while (st >= 1) {
                    lastState <- thisTheseStates[, st]
                    betaStates <- betaStates[lastState == 1] - betaStates[lastState == 0]
                    thisTheseStates <- unique(thisTheseStates[, -st, drop = FALSE])
                    st <- st - 1
                }
                thisProfits <- rowSums(profitsCond[, q, drop = FALSE])

                res <- (-1)^(nMarkets - 1) * (prod(thisProfits[-1]) / (thisProfits[1]^(nMarkets - 1))) * betaStates

                return(res)
            })

            gFun <- sapply(firmVec, function(q, pre = preMerger) {
                s0_focal <- apply(theseStates, 1, s0Focal, i = q)
                betaStates <- 2 / factorial(nMarkets) * s0_focal^(nMarkets)
                betaStates <- beta(1, 2) * (betaStates - 2 / factorial(nMarkets + 1) * s0_focal^(nMarkets + 1))

                st <- ncol(theseStates)
                thisTheseStates <- theseStates
                while (st >= 1) {
                    lastState <- thisTheseStates[, st]
                    betaStates <- betaStates[lastState == 1] - betaStates[lastState == 0]
                    thisTheseStates <- unique(thisTheseStates[, -st, drop = FALSE])
                    st <- st - 1
                }

                thisProfits <- rowSums(profitsCond[, q, drop = FALSE])

                res <- (-1)^(nMarkets + 1) * (prod(thisProfits[-1]) / (thisProfits[1]^(nMarkets - 1))) * betaStates
                return(res)
            })

            if (nMarkets > 1) {
                otherFun <- function(q, wgt = .5, pre = preMerger) {
                    theseStatesLocal <- theseStates[theseStates$Var1 == 1, , drop = FALSE]

                    s0_focal <- apply(theseStatesLocal, 1, s0Focal, i = q)
                    if (!pre && q == 2) q <- 2:1
                    thisProfits <- rowSums(profitsCond[, q, drop = FALSE])
                    betaStates <- s0_focal^(nMarkets - 1) / factorial(nMarkets - 1)

                    st <- ncol(theseStatesLocal)
                    while (st >= 2) {
                        lastState <- theseStatesLocal[, st]
                        betaStates <- betaStates[lastState == 1] - betaStates[lastState == 0]
                        theseStatesLocal <- unique(theseStatesLocal[, -st, drop = FALSE])
                        st <- st - 1
                    }

                    if (nMarkets == 2) {
                        res <- betaStates
                    } else {
                        res <- (-1)^(nMarkets - 1) * (prod(thisProfits[-(1:2)]) / (thisProfits[1]^(nMarkets - 2))) * betaStates
                    }

                    res <- res + wgt * (thisProfits[2] / thisProfits[1])
                    return(res)
                }

                tFun <- beta(2, 2) * sapply(firmVec, otherFun, wgt = 1 / 2)
                rFun <- beta(1, 2) * sapply(firmVec, otherFun, wgt = 2 / 3)
            }
        } else {
            hFun <-
                sapply(firmVec, function(g) {
                    cubature::cubintegrate(function(x, f = g) {
                        x <- as.matrix(x)
                        matrix(apply(x, 2, function(s) {
                            s0_focal <- s0Focal(s, f)
                            res <- beta_h_focal_integral(s0_focal, shape1 = shareOutParm[1], shape2 = shareOutParm[2]) * prod(bDens(s))
                            return(res)
                        }), ncol = col(x))
                    },
                    lower = lowerB, upper = upperB, nVec = control.cub$nVec,
                    method = control.cub$method,
                    relTol = control.cub$relTol, absTol = control.cub$absTol,
                    maxEval = control.cub$maxEval
                    )$integral
                })

            gFun <-
                sapply(firmVec, function(g) {
                    cubature::cubintegrate(function(x, f = g) {
                        x <- as.matrix(x)
                        matrix(apply(x, 2, function(s) {
                            s0_focal <- s0Focal(s, f)

                            beta_g_focal_integral(s0_focal, shape1 = shareOutParm[1], shape2 = shareOutParm[2]) * prod(bDens(s))
                        }), ncol = col(x))
                    },
                    lower = lowerB, upper = upperB, nVec = control.cub$nVec,
                    method = control.cub$method,
                    relTol = control.cub$relTol, absTol = control.cub$absTol,
                    maxEval = control.cub$maxEval
                    )$integral
                })

            if (nMarkets > 1) {
                tFun <-
                    sapply(firmVec, function(g) {
                        cubature::cubintegrate(function(x, f = g) {
                            x <- as.matrix(x)
                            matrix(apply(x, 2, function(s) {
                                integrateDerivOtherFun(s, f) * prod(bDens(s))
                            }), ncol = col(x))
                        },
                        fDim = 1,
                        lower = lowerB, upper = upperB, nVec = control.cub$nVec,
                        method = control.cub$method,
                        relTol = control.cub$relTol, absTol = control.cub$absTol,
                        maxEval = control.cub$maxEval
                        )$integral
                    })

                rFun <-
                    sapply(firmVec, function(g) {
                        cubature::cubintegrate(function(x, f = g) {
                            x <- as.matrix(x)
                            matrix(apply(x, 2, function(s) {
                                integrateInsideOtherFun(s, f) * prod(bDens(s))
                            }), ncol = col(x))
                        },
                        fDim = 1,
                        lower = lowerB, upper = upperB, nVec = control.cub$nVec,
                        method = control.cub$method,
                        relTol = control.cub$relTol, absTol = control.cub$absTol,
                        maxEval = control.cub$maxEval
                        )$integral
                    })
            }
        }

        if (nMarkets > 1) {
            shareOutOther <- tFun / rFun
            shareOutOther[shareOutOther > 1] <- 1
            shareOutOther[shareOutOther < 0] <- 0
        }

        shareOutFocal <- hFun / gFun
        shareOutFocal[shareOutFocal > 1] <- 1
        shareOutFocal[shareOutFocal < 0] <- 0

        SharesOutInt <- matrix(NA, nrow = nMarkets, ncol = nFirms, dimnames = list(1:nMarkets, firmVec))
        SharesOutInt[focal, ] <- shareOutFocal
        if (nMarkets > 1) SharesOutInt[-focal, ] <- shareOutOther

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

        ## Find price changes that set FOCs equal to 0
        minResult <- BB::BBsolve(priceStart, FOC, quiet = TRUE, control = object@control.equ, ...)

        if (minResult$convergence != 0) {
            warning("'calcPrices' nonlinear solver may not have successfully converged. 'BBsolve' reports: '", minResult$message, "'")
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

            startMC <- -1 * (object@margins - object@prices)
            startMC[is.na(startMC)] <- max(startMC, na.rm = TRUE)

            minD <- function(c) {
                object@mcPre <- c
                thisMargin <- calcMargins(object, preMerger = TRUE, level = TRUE)

                measure <- object@pricePre - c - thisMargin
                measure <- (measure / object@pricePre)^2
                return(sum(measure))
            }

            lowerB <- rep(0, length(startMC))
            upperB <- object@prices

            minMC <- stats::optim(startMC, minD, method = "L-BFGS-B", lower = lowerB, upper = upperB)

            mc <- minMC$par
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

            shares <- t(rowsum(t(shares), group = owner, na.rm = TRUE))
            shares <- shares[, owner]
        } else {
            shares <- tapply(shares, owner, sum, na.rm = TRUE)
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
