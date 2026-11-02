# financial

Financial merger models extend the structural-fit lifecycle in antitrust.
The debt and bank-liquidity families have distinct economic equations and
supported operations. Install antitrust 0.99.43 or newer and the dependencies
listed in DESCRIPTION, including cubature, before installing this package.

The new bank economic gate independently checks ownership-adjusted loan and
deposit FOCs for competition-only and pooled-liquidity merger scenarios.

Use `bash ci/build-check.sh /path/to/antitrust` for an isolated source check.
Ordinary development runs bootstrap from the `refactor` branch and record the
resolved commit; set `ANTITRUST_REF` to select another local development ref.
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
