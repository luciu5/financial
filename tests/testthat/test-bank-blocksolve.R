## Architecture-lead verification: the block-diagonal Nash-in-rates FOC solve
## (.bank_solve_markup_block) is a LOSSLESS generalization of the legacy
## exposure script's per-bank scalar division + 2x2 solve. The legacy code only
## ever handled a single 2-bank merging pair; the migration generalizes to
## arbitrary ownership groups. No captured multi-bank merger fixture exists, so
## this test pins the equivalence the generalization rests on: for any invertible
## ownership-weighted FOC matrix, the block solver equals sign * solve(foc, Q),
## and reduces to scalar division when every bank stands alone.

test_that("block-solve equals full ownership-system solve (mixed 2+1 merger)", {
  solveblk <- financial:::.bank_solve_markup_block
  deriv <- matrix(c(-2.0, 0.3, 0.0,
                     0.4, -1.8, 0.0,
                     0.0, 0.0, -2.5), 3, 3, byrow = TRUE)
  owner <- matrix(c(1, 1, 0,
                    1, 1, 0,
                    0, 0, 1), 3, 3, byrow = TRUE)  # banks 1&2 merge, 3 standalone
  Q <- c(0.5, 0.3, 0.7)
  foc <- owner * t(deriv)
  expect_equal(solveblk(foc, Q, sign = -1), as.numeric(-solve(foc, Q)), tolerance = 1e-12)
})

test_that("block-solve equals full solve (3-way merger, single dense block)", {
  solveblk <- financial:::.bank_solve_markup_block
  deriv <- matrix(c(-2.0, 0.3, 0.2, 0.4, -1.8, 0.25, 0.15, 0.35, -2.5), 3, 3, byrow = TRUE)
  owner <- matrix(1, 3, 3)
  Q <- c(0.5, 0.3, 0.7)
  foc <- owner * t(deriv)
  expect_equal(solveblk(foc, Q, sign = -1), as.numeric(-solve(foc, Q)), tolerance = 1e-12)
})

test_that("block-solve reduces to legacy per-bank scalar division when all standalone", {
  solveblk <- financial:::.bank_solve_markup_block
  deriv <- matrix(c(-2.0, 0.3, 0.0, 0.4, -1.8, 0.0, 0.0, 0.0, -2.5), 3, 3, byrow = TRUE)
  foc <- diag(3) * t(deriv)
  Q <- c(0.5, 0.3, 0.7)
  expect_equal(solveblk(foc, Q, sign = -1), as.numeric(-1 * Q / diag(foc)), tolerance = 1e-12)
})
