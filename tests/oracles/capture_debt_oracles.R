# Capture pre-migration golden-master oracles for the Debt module (mergersdebt @ ea727430).
# READ-ONLY against MergerDebt / antitrust source. Writes fixtures under tests/oracles/.
#
# Run with:
#   Rscript tests/oracles/capture_debt_oracles.R
#
# from /fsr/home/m1cst00/Projects/ioverse/financial (or any cwd; paths below are absolute).

suppressMessages(library(antitrust))
suppressMessages(library(dplyr))
suppressMessages(devtools::load_all("/fsr/home/m1cst00/Projects/completed/MergerDebt", quiet = TRUE))

OUT_DIR <- "/fsr/home/m1cst00/Projects/ioverse/financial/tests/oracles"
SEED <- 42L

cat("antitrust version:", as.character(packageVersion("antitrust")), "\n")
cat("MergerDebt SHA (recorded from git log): ea727430509851c6fe972dbc0a8a56935e54dac7\n")

# ---------------------------------------------------------------------------
# Shared example inputs: corrected 3-firm / 2-market example from
# R/DebtFunctions.R:59-82 (original doc example is broken: mktSize <- 1 must
# be mktSize <- c(1,1) since nMarkets == 2).
# ---------------------------------------------------------------------------
nprods   <- 3
nmarkets <- 2
marketId <- rep(1:nmarkets, nprods)
shares   <- c(0.156, 0.418, 0.616, 0.424, 0.229, 0.158)
prices   <- c(97.303, 119.083, 94.103, 147.905, 93.113, 100.935)
margins  <- c(17.163, 84.026, 26.451, 103.05, 17.316, 64.105) / prices
ownerPre  <- ownerPost <- as.character(rep(c(1, 2, 3), each = nmarkets))
ownerPost[ownerPost == "2"] <- "1"
mktSize  <- c(1, 1)

debt_zero <- rep(0, length(prices))
debt_low  <- c(22.69, NA, 51.002, NA, 2.922, NA)      # original (corrected) example debt
debt_high <- c(60, NA, 90, NA, 40, NA)                # stress case: high debt

grid_results <- list()

run_case <- function(debt, density, shareOutParm = NULL, label) {
  cat("Running case:", label, "\n")
  set.seed(SEED)
  args <- list(
    prices = prices, shares = shares, margins = margins,
    ownerPre = ownerPre, ownerPost = ownerPost,
    debtPre = debt, debtPost = debt,
    marketID = marketId, mktSize = mktSize,
    density = density
  )
  if (!is.null(shareOutParm)) args$shareOutParm <- shareOutParm

  fit <- withCallingHandlers(
    do.call(logit.debt, args),
    warning = function(w) {
      cat("  WARNING [", label, "]: ", conditionMessage(w), "\n", sep = "")
      invokeRestart("muffleWarning")
    }
  )

  sharesPre  <- MergerDebt::calcShares(fit, preMerger = TRUE)
  sharesPost <- MergerDebt::calcShares(fit, preMerger = FALSE)
  summ_market <- tryCatch(
    as.data.frame(summary(fit, market = TRUE)),
    error = function(e) { cat("  summary(market=TRUE) ERROR:", conditionMessage(e), "\n"); NULL }
  )

  list(
    label = label,
    inputs = list(
      prices = prices, shares = shares, margins = margins,
      ownerPre = ownerPre, ownerPost = ownerPost,
      debtPre = debt, debtPost = debt,
      marketID = marketId, mktSize = mktSize,
      density = density, shareOutParm = shareOutParm,
      seed = SEED
    ),
    pricePre = fit@pricePre,
    pricePost = fit@pricePost,
    slopes = fit@slopes,
    shareOutParm_calibrated = fit@shareOutParm,
    sharesPre = sharesPre,
    sharesPost = sharesPost,
    mcPre = fit@mcPre,
    mcPost = fit@mcPost,
    summary_market = summ_market
  )
}

grid_results[["zero_uniform"]]  <- run_case(debt_zero, "uniform", c(1, 1), "zero-debt / uniform")
grid_results[["low_uniform"]]   <- run_case(debt_low,  "uniform", c(1, 1), "low-debt (doc example) / uniform")
grid_results[["high_uniform"]]  <- run_case(debt_high, "uniform", c(1, 1), "high-debt / uniform")
grid_results[["zero_beta"]]     <- run_case(debt_zero, "beta", NULL, "zero-debt / beta (calibrated shareOutParm)")
grid_results[["low_beta"]]      <- run_case(debt_low,  "beta", NULL, "low-debt (doc example) / beta (calibrated shareOutParm)")
grid_results[["high_beta"]]     <- run_case(debt_high, "beta", NULL, "high-debt / beta (calibrated shareOutParm)")

saveRDS(grid_results, file.path(OUT_DIR, "debt_goldenmaster_grid.rds"))

# flatten to a CSV manifest for human readability
manifest_rows <- lapply(names(grid_results), function(nm) {
  r <- grid_results[[nm]]
  data.frame(
    case = nm,
    label = r$label,
    density = r$inputs$density,
    debt_sum = sum(r$inputs$debtPre, na.rm = TRUE),
    alpha = paste(round(r$slopes$alpha, 6), collapse = ";"),
    shareOutParm1 = r$shareOutParm_calibrated[1],
    shareOutParm2 = r$shareOutParm_calibrated[2],
    pricePre = paste(round(r$pricePre, 4), collapse = ";"),
    pricePost = paste(round(r$pricePost, 4), collapse = ";")
  )
})
manifest <- do.call(rbind, manifest_rows)
write.csv(manifest, file.path(OUT_DIR, "debt_goldenmaster_grid_manifest.csv"), row.names = FALSE)
cat("Saved debt_goldenmaster_grid.rds + manifest CSV\n\n")

# ---------------------------------------------------------------------------
# Zero-debt directional sanity: logit.debt(debtPre=debtPost=0, uniform,
# shareOutParm=c(1,1)) vs antitrust::logit() on the SAME firms/products.
# This is a SIGN/DIRECTION check only (debt and no-debt models are not
# expected to produce identical numeric outputs -- the demand/calibration
# machinery differs even at debt=0), not an exact-match oracle.
# ---------------------------------------------------------------------------
cat("Running zero-debt directional sanity check (single market, 3 single-product firms)\n")

sm_shares   <- c(0.3, 0.4, 0.3)
sm_prices   <- c(10, 12, 9)
sm_margins  <- c(0.5, 0.55, 0.45)
sm_ownerPre  <- c("1", "2", "3")
sm_ownerPost <- c("1", "1", "3")
sm_debt_zero <- rep(0, 3)

set.seed(SEED)
debt_zero_fit <- logit.debt(
  sm_prices, sm_shares, sm_margins, sm_ownerPre, sm_ownerPost,
  debtPre = sm_debt_zero, debtPost = sm_debt_zero,
  density = "uniform", shareOutParm = c(1, 1)
)

set.seed(SEED)
plain_logit_fit <- logit(
  sm_prices, sm_shares, sm_margins,
  ownerPre = sm_ownerPre, ownerPost = sm_ownerPost
)

zero_debt_sanity <- list(
  note = paste(
    "SIGN/DIRECTION check only: logit.debt() at debt=0 uses different demand",
    "calibration machinery (Beta/uniform outside-share integrals) than plain",
    "antitrust::logit(); exact numeric match is NOT expected. Both should agree",
    "in DIRECTION (all merging-party and rival prices rise post-merger)."
  ),
  inputs = list(
    prices = sm_prices, shares = sm_shares, margins = sm_margins,
    ownerPre = sm_ownerPre, ownerPost = sm_ownerPost, seed = SEED
  ),
  logit_debt = list(
    pricePre = debt_zero_fit@pricePre,
    pricePost = debt_zero_fit@pricePost,
    pctChange = 100 * (debt_zero_fit@pricePost / debt_zero_fit@pricePre - 1)
  ),
  antitrust_logit = list(
    pricePre = plain_logit_fit@pricePre,
    pricePost = plain_logit_fit@pricePost,
    pctChange = 100 * (plain_logit_fit@pricePost / plain_logit_fit@pricePre - 1)
  )
)

saveRDS(zero_debt_sanity, file.path(OUT_DIR, "debt_zerodebt_direction_sanity.rds"))

sanity_df <- data.frame(
  product = c("Prod1(merging)", "Prod2(merging)", "Prod3(rival)"),
  logitDebt_pctChange = as.numeric(zero_debt_sanity$logit_debt$pctChange),
  antitrustLogit_pctChange = as.numeric(zero_debt_sanity$antitrust_logit$pctChange)
)
write.csv(sanity_df, file.path(OUT_DIR, "debt_zerodebt_direction_sanity.csv"), row.names = FALSE)
cat("Saved debt_zerodebt_direction_sanity.rds + .csv\n\n")

# ---------------------------------------------------------------------------
# Single-market closed form: nMarkets == 1, hand-verifiable
# s0Focal = 1 - debt/profit
# ---------------------------------------------------------------------------
cat("Running single-market closed-form s0Focal capture\n")

cf_shares  <- c(0.3, 0.4, 0.3)
cf_prices  <- c(10, 12, 9)
cf_margins <- c(0.5, 0.55, 0.45)
cf_ownerPre  <- c("1", "2", "3")
cf_ownerPost <- c("1", "1", "3")
cf_debt <- c(0.5, 0.6, 0.4)   # modest debt -> s0Focal lands in (0,1), hand-verifiable

set.seed(SEED)
cf_fit <- logit.debt(
  cf_prices, cf_shares, cf_margins, cf_ownerPre, cf_ownerPost,
  debtPre = cf_debt, debtPost = cf_debt,
  density = "uniform", shareOutParm = c(1, 1)
)

alpha <- cf_fit@slopes$alpha
mval  <- cf_fit@slopes$meanval
M     <- cf_fit@mktSize
pricePre <- cf_fit@pricePre
mcPre    <- cf_fit@mcPre
sCond <- exp(mval + alpha * pricePre)
sCond <- sCond / sum(sCond)
marginLevel <- pricePre - mcPre
profitsCond <- M * marginLevel * sCond
names(profitsCond) <- cf_ownerPre

s0Focal_manual <- 1 - cf_debt / profitsCond
s0Focal_internal <- MergerDebt::calcShares(cf_fit, preMerger = TRUE, outSideOnly = TRUE)

single_market_closedform <- list(
  inputs = list(
    prices = cf_prices, shares = cf_shares, margins = cf_margins,
    ownerPre = cf_ownerPre, ownerPost = cf_ownerPost,
    debtPre = cf_debt, debtPost = cf_debt,
    density = "uniform", shareOutParm = c(1, 1), seed = SEED
  ),
  calibrated_alpha = alpha,
  calibrated_meanval = mval,
  pricePre = pricePre,
  mcPre = mcPre,
  sCond = sCond,
  profitsCond = profitsCond,
  s0Focal_manual_1_minus_debt_over_profit = s0Focal_manual,
  s0Focal_internal_calcShares_outSideOnly = s0Focal_internal,
  note = paste(
    "nMarkets == 1 closed form: s0Focal(i) = 1 - debt[i] / profitsCond[i].",
    "profitsCond[i] = mktSize * (pricePre[i]-mcPre[i]) * sCond[i] aggregated to firm i",
    "(here single-product firms so no aggregation needed).",
    "s0Focal_manual should match calcShares(outSideOnly=TRUE) after clipping to [0,1]."
  )
)

saveRDS(single_market_closedform, file.path(OUT_DIR, "debt_singlemarket_closedform.rds"))

cf_df <- data.frame(
  firm = cf_ownerPre,
  pricePre = pricePre,
  mcPre = mcPre,
  sCond = sCond,
  profitsCond = as.numeric(profitsCond),
  debt = cf_debt,
  s0Focal_manual = as.numeric(s0Focal_manual)
)
write.csv(cf_df, file.path(OUT_DIR, "debt_singlemarket_closedform.csv"), row.names = FALSE)
cat("Saved debt_singlemarket_closedform.rds + .csv\n\n")

cat("=== Debt module oracle capture complete ===\n")
