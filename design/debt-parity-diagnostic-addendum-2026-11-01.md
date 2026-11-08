# Debt parity diagnostic addendum (2026-11-01)

This addendum preserves the historical release-test findings and records how
the debt checks distinguish valid economic results from legacy solver traps.
The oracle RDS/CSV files and their original capture notes remain unchanged.

## Provenance and environment

The oracle README identifies MergerDebt commit
`ea727430509851c6fe972dbc0a8a56935e54dac7` and records capture under R 4.4.1
and antitrust 0.99.35. The available checkout at
`/mnt/d/Projects/mergers_debt/MergerDebt` is at `25261389af099a9e03e1b105b4f7aebca266861e`;
`git cat-file` reports that the requested `ea727...` object is absent. The
comparison below therefore uses the available local MergerDebt source only
and makes no claim that it is the frozen source commit.

The release library used here has R 4.6.1, antitrust 0.99.42, BB 2026.1.0,
and cubature 2.1.4.1. This differs from the documented oracle capture in R
and antitrust versions. The installed financial release source and the
available local legacy source were run with that same library and inputs.

## Historical test result body

Before changing the assertions, extended validation reported 163 passes and
four failures. The raw output was:

```text
zero_uniform pricePost
  actual:   117.57501 186.16687 64.00551 187.38994 98.01836 103.42608
  expected: 117.57501 186.17977 64.03092 187.41198 97.98258 103.42189

zero_uniform sharesPre
  actual:   NaN NaN 0.61255 0.30198 0.00000 0.11581
  expected: NaN NaN 0.61242 0.30198 0.00000 0.11580

zero_uniform sharesPost
  actual:   0.00000 0.19812 0.00000 0.31582 0.00000 0.21990
  expected: 0.00000 0.19812 0.00000 0.31577 0.00000 0.21996

low_beta shareOutParm
  actual:   4.1456377 41.2884711
  expected: 4.1456436 41.2885252

TOTAL passed 163 failed 4 errors 0 warnings 0 skips 0
```

The `zero_uniform` oracle README already records a BBsolve non-convergence
warning for this debt-zero/uniform case. In the available local legacy source,
the same inputs and environment emitted `Lack of improvement in objective
function` and returned the same migrated values. Its merger-party adjusted
shares are non-finite. Those outputs do not identify an equilibrium and are
not valid parity targets. The C legacy-trap check now requires the solver
warning and deliberately makes no assertion about those prices or shares.

For `low_beta`, the available local legacy source and the migrated package
both returned alpha `(-0.0549835762533392, -0.0133513332236401)` and Beta
shapes `(4.14563768791511, 41.2884710501388)` in the current environment.
The oracle captured shapes `(4.14564357363476, 41.2885251545012)`. Comparing
the raw shape parameters at `1e-6` therefore rejected two implementations
that agree under the same runtime. The distributions' mean and variance
differ by `9.07e-9` and `2.15e-9`; their CDFs at default-threshold cutoffs
`0.05, 0.10, 0.25, 0.50` differ by at most `2.67e-7`. These economically
observable quantities remain checked against the historical capture at the
unchanged `1e-6` tolerance.

The same `low_beta` run produces NaN default-adjusted shares for the two
merging firms and post-merger prices equal to the starting prices. The test
does not present those values as valid equilibrium outcomes. It checks the
calibrated price sensitivity, the Beta law, and the independent identity
that conditional inside shares normalize to observed shares within each
market.

## High-debt Beta follow-up

The initial grid disposition also treated `high_beta` as valid because its
fixture comparisons did not fail. Review found that this was a false pass:
the fixture and current run both have zero finite default-adjusted shares
across the six products, while post-merger prices equal pre-merger prices.
Matching NaNs and unchanged starting prices do not establish an equilibrium.
`high_beta` is therefore excluded from B price/share parity and its
equilibrium predictions are classified C (legacy trap). The test does not
assert that NaNs are desirable and does not require a warning that the source
does not emit.

The calibration primitives remain checkable. Under the current environment,
the fitted alpha and Beta shapes equal the stored capture; the selected Beta
CDF values and moments agree at the unchanged `1e-6` tolerance. Its
conditional inside shares are finite, sum to one by market, and equal the
independently computed logit softmax. These checks validate calibration only;
they do not validate the high-debt outside-share integrals or equilibrium.

## Test disposition

- **A, permanent:** low-debt demand calibration checks the price slopes,
  Beta mean/variance and selected CDF values, and conditional-share identity.
  High-debt Beta calibration checks alpha, Beta moments/CDF, and the
  conditional-logit softmax identity only. A separate low-uniform case
  independently checks the multi-market Bertrand FOC residual under both
  ownership structures. These tests target changes in default probabilities,
  share normalization, ownership aggregation, or price equilibrium that
  object-construction checks cannot detect.
- **B, temporary:** raw numeric parity remains for the finite-output
  `low_uniform`, `high_uniform`, and `zero_beta` grid cases. The test comment
  sets its sunset at 2027-11-01, or earlier when the capture toolchain is
  restored and independent checks replace the cross-version parity layer.
- **C, legacy trap:** zero-uniform's test checks BBsolve's known
  non-convergence warning and does not freeze its returned root or shares.
  Low-beta and high-beta equilibrium prices/default-adjusted shares remain
  unvalidated legacy outputs; their calibration-only checks do not certify
  those equilibrium calculations.

The multi-market FOC check independently recomputes ownership aggregation
and markup residuals conditional on public `calcShares()` outputs. It does
not independently validate the multi-market default integrals.

No source economics, solver, or oracle fixtures were changed for this
diagnosis. The existing `calcPrices` warning is the non-convergence
diagnostic. The valid single-market closed-form and zero-debt direction tests
remain in place as independent checks.

## Validation after assertion changes

Focused extended parity validation completed with 41 passes, 0 failures,
0 warnings, and 0 skips. The package was then built with
`R CMD build --no-build-vignettes financial` and checked with extended tests
enabled using the staged antitrust 0.99.43 package in a separate temporary
library. `R CMD check --no-manual --no-build-vignettes financial_0.1.0.tar.gz`
completed with status `OK`; `testthat.R` reported 168 passes, 0 failures,
0 warnings, and 0 skips.

## Numerical repair follow-up (2026-11-06)

The sections above describe the pre-repair release diagnosis. The subsequent
repair evaluates the same inherited conditional-share equations on their true
support. Uniform thresholds saturate at zero and one, concentrated Beta laws
are integrated in probability space, and a triangular transformation resolves
small solvent regions before cubature. Every market has its own moments;
changing the focal market or a zero focal profit no longer changes economics.
Beta moment prefactors avoid underflow in ratios of Beta normalizations.

Calibration now uses the Beta mean `shape1 / (shape1 + shape2)` in other-market
thresholds, and cost initialization converts proportional margins to dollars.
Invalid model residuals are not dropped. Returned price and cost solutions
must satisfy finite residual checks; Newton refinement may improve a BB root
before acceptance. Historical numerical outputs and shape parameters affected
by these repairs are no longer parity acceptance targets. Oracle files remain
unchanged as provenance. Permanent A tests use independent moments, simplex
identities, ownership FOCs and failure acceptance. High-debt captures with no
solvent region are C legacy traps and now fail explicitly.

Errors inherit `financial_zero_survival`, `financial_integration_failure`, or
`financial_equilibrium_failure`. Zero solvent mass is undefined conditioning,
not a zero default-adjusted share or a uniquely identified corner equilibrium.
The repair does not derive new limited-liability payoff economics for that
regime. Integration failures and unverified roots similarly return no alleged
equilibrium. Existing numerical tolerances are retained or tightened.

Compatibility: debt calibration now explicitly requires complete finite product
margins. Legacy missing-margin inputs propagated unknown costs into firm profits
and silently omitted the resulting invalid values; they did not identify the
required default integrals. Such inputs now raise `financial_equilibrium_failure`
with the affected product positions. This does not restrict bank calibration.
