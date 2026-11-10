# financial

Financial merger models extend the structural-fit lifecycle in antitrust.
The debt and bank-liquidity families have distinct economic equations and
supported operations. Install antitrust 0.99.43 or newer and the dependencies
listed in DESCRIPTION, including cubature, before installing this package.


## Install and run

```r
source("https://raw.githubusercontent.com/luciu5/competitiontoolbox/master/tools/install_iovrse.R")
install_iovrse(packages = "financial")
library(financial)
```

The public installer includes compatible antitrust and ordinary dependencies.
These deterministic examples require no CmdStan installation or Bayesian fit.
`bank_fit()` accepts named point estimates or a matrix/data.frame of draws;
`posterior` is needed only to ingest its draw objects or CmdStanR objects directly.

### Task guides

- [Debt, default risk, and one merger](vignettes/debt-default-merger.Rmd): product/firm payment units, prices and default/survival comparisons.
- [Bank merger rates and liquidity pooling](vignettes/bank-liquidity-merger.Rmd): supplied point estimates, loan/deposit rates and the competition/pooling decomposition.

These are independent starting points. The ordinary installer skips vignette
building; the source guides above remain readable without Pandoc. Source
bundles built with vignettes also provide `browseVignettes("financial")`.

### A valid single-market debt merger

All product margins are required. This small Beta-law example is used by the
lifecycle tests; the historical multi-market low-beta example remains diagnostic
evidence rather than the advertised starting point.

```r
debt <- logit.debt(
  prices = c(10, 10, 10), shares = c(.3, .3, .4),
  margins = c(.5, .5, .5),
  ownerPre = c("1", "2", "3"), ownerPost = c("1", "1", "3"),
  debtPre = c(.5, .5, .5), mktSize = 1, density = "beta"
)
stopifnot(all(is.finite(debt@pricePost)))
debt@pricePost
```

Use `debt_fit()` with the same inputs for a structural-fit wrapper, then
`simulate()` to obtain default-probability and survival-mass changes in
`fit@diagnostics$uncertainty_delta`.

### A bank merger from point estimates

The bank family has separate loan/deposit pricing and liquidity equations.
Supply structural point estimates and observed bank-market inputs; no estimation
is performed by the following example. Explicit total assets determine the
asset-weighted pooling rule.

```r
bank_id <- rep(c("A", "B", "C"), each = 2)
data <- list(
  bank_id = bank_id, market_id = rep(c("m1", "m2"), 3),
  s_cond_L = c(.4, .3, .35, .4, .25, .3),
  s_cond_D = c(.3, .35, .4, .3, .3, .35),
  market_size_L = rep(100, 6), market_size_D = rep(120, 6),
  r_L = rep(.05, 6), r_D = rep(.02, 6), r_f = rep(.03, 6),
  W_L = matrix(0, 6, 0), W_D = matrix(0, 6, 0),
  H_out_L = matrix(1, 6, 1), H_out_D = matrix(1, 6, 1),
  out_iv_L = rep(0, 6), out_iv_D = rep(0, 6),
  market_state_region_id = rep(1L, 6), group_id = bank_id,
  Z_psi = matrix(0, 3, 1), equity = c(A = 10, B = 8, C = 12),
  liquid_assets = c(A = 5, B = 4, C = 6), assets = c(A = 150, B = 120, C = 180)
)
parameters <- c(
  alpha_bar_L = log(3), alpha_bar_D = log(3),
  sigma_alpha_state_L = 0, sigma_alpha_state_D = 0,
  `alpha_state_raw_L[1]` = 0, `alpha_state_raw_D[1]` = 0,
  `theta_out_L[1]` = 0, `theta_out_D[1]` = 0,
  alpha_out_L = -1.7, alpha_out_D = -1.7,
  log_phi_bar_L = log(15), log_phi_bar_D = log(15), gamma_psi = 0
)
bank <- bank_fit(parameters, data)
merger <- simulate(bank, ownerPost = c(A = "AB", B = "AB", C = "C"))
merger@diagnostics$counterfactual$rates$full_L
merger@diagnostics$counterfactual$scenario_diff
```

The counterfactual reports competition-only and pooled-liquidity rates, plus
their difference. See `?bank_fit` and `?simulate` for the input and output
contracts. The independent bank economic tests verify ownership-adjusted FOCs;
finite prices alone are not an equilibrium certificate.

## Development and release checks

The new bank economic gate independently checks ownership-adjusted loan and
deposit FOCs for competition-only and pooled-liquidity merger scenarios.

Use `bash ci/build-check.sh /path/to/antitrust` for an isolated source check.
Ordinary development runs bootstrap from the antitrust `master` branch and
record the resolved commit; set `ANTITRUST_REF` to select another local
development ref.
For release certification, set `RELEASE_CERTIFICATION=true`
and `ANTITRUST_RELEASE_SHA` to the full 40-character candidate SHA; mutable
refs such as `main` and `refactor` are rejected. Set
`FINANCIAL_EXTENDED_TESTS=1` for extended validation. The runner archives the
selected commit and records requested/resolved identity and package hashes
without changing source Git state. Hosted CI exposes the same certification
mode and SHA input.

Missing cubature prevents package-level verification; standalone accounting or
Beta-integral checks do not certify the full package.

## Debt model validation

Some historical multi-market debt designs return nonfinite default-adjusted
shares or nonconverged prices. The zero-uniform, low-beta and high-beta legacy
examples are not certified equilibrium predictions. Their stored fixtures are
preserved, while calibration-only Beta-law checks and valid ownership/price
conditions are tested separately. A matching invalid legacy output is not
evidence of an economic equilibrium. See
[the debt parity addendum](design/debt-parity-diagnostic-addendum-2026-11-01.md)
for the diagnosed cases and the bounded source comparison.

Debt equilibrium calculations reject undefined default conditioning with a
`financial_zero_survival` error. Quadrature that fails its error bounds raises
`financial_integration_failure`; unverified calibration or price roots raise
`financial_equilibrium_failure`. Historical NaN captures are diagnostic evidence,
not successful equilibria. See the debt numerical repair follow-up in
`design/debt-parity-diagnostic-addendum-2026-11-01.md`.

Compatibility: debt calibration now explicitly requires complete finite product
margins. Legacy missing-margin inputs propagated unknown costs into firm profits
and silently omitted the resulting invalid values; they did not identify the
required default integrals. Such inputs now raise `financial_equilibrium_failure`
with the affected product positions. This does not restrict bank calibration.
