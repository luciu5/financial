## Architecture-lead verification: the block-diagonal Nash-in-rates FOC solve
## (.bank_solve_markup_block) is a LOSSLESS generalization of the legacy
## exposure script's per-bank scalar division + 2x2 solve. The legacy code only
## ever handled a single 2-bank merging pair; the migration generalizes to
## arbitrary ownership groups. No captured multi-bank merger fixture exists, so
## this test pins the equivalence the generalization rests on: for any invertible
## ownership-weighted FOC matrix, the block solver equals sign * solve(foc, Q),
## and reduces to scalar division when every bank stands alone.
##
## CRITICAL FIX (red-team, 2026-09): blocks must be the OWNERSHIP GROUP (an
## equivalence relation, hence a true block partition), not inferred from the
## FOC matrix's zero pattern. A one-hop-neighbor scan is WRONG for a
## chain-connected group of 3+ commonly-owned banks that lack a direct
## cross-derivative (e.g. no shared market between the first and last bank):
## it splits the group into overlapping 2-bank blocks that overwrite each
## other, violating the true joint FOC by a material margin. Verified: before
## the fix, the block solve's residual against the true joint solve was as
## large as 0.15 on the synthetic chain below (foc %*% (-blocksolve) - Q),
## vs machine epsilon for solve(foc, Q) directly. See "chain-connected
## ownership group" test below -- this is the regression test for that bug.

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
  blockId <- c("A", "A", "B")
  expect_equal(solveblk(foc, Q, sign = -1, blockId = blockId), as.numeric(-solve(foc, Q)), tolerance = 1e-12)
})

test_that("block-solve equals full solve (3-way merger, single dense block)", {
  solveblk <- financial:::.bank_solve_markup_block
  deriv <- matrix(c(-2.0, 0.3, 0.2, 0.4, -1.8, 0.25, 0.15, 0.35, -2.5), 3, 3, byrow = TRUE)
  owner <- matrix(1, 3, 3)
  Q <- c(0.5, 0.3, 0.7)
  foc <- owner * t(deriv)
  blockId <- c("A", "A", "A")
  expect_equal(solveblk(foc, Q, sign = -1, blockId = blockId), as.numeric(-solve(foc, Q)), tolerance = 1e-12)
})

test_that("block-solve reduces to legacy per-bank scalar division when all standalone", {
  solveblk <- financial:::.bank_solve_markup_block
  deriv <- matrix(c(-2.0, 0.3, 0.0, 0.4, -1.8, 0.0, 0.0, 0.0, -2.5), 3, 3, byrow = TRUE)
  foc <- diag(3) * t(deriv)
  Q <- c(0.5, 0.3, 0.7)
  blockId <- c("A", "B", "C")
  expect_equal(solveblk(foc, Q, sign = -1, blockId = blockId), as.numeric(-1 * Q / diag(foc)), tolerance = 1e-12)
})

test_that("block-solve is correct for a chain-connected 3-bank ownership group (regression)", {
  ## Banks 1,2,3 are ALL commonly owned (one merger of all three), but bank 1
  ## and bank 3 have NO direct cross-derivative (foc[1,3] == foc[3,1] == 0 --
  ## e.g. no shared market), while 1-2 and 2-3 do. A one-hop-neighbor block
  ## finder would wrongly split this into blocks {1,2} and {2,3}; the correct
  ## block is the single ownership group {1,2,3}.
  solveblk <- financial:::.bank_solve_markup_block
  deriv <- matrix(c(-2.0, 0.5, 0.0,
                      0.6, -3.0, 0.7,
                      0.0, 0.8, -2.5), 3, 3, byrow = TRUE)
  owner <- matrix(1, 3, 3)  # all three commonly owned
  foc <- owner * t(deriv)   # foc[1,3] == foc[3,1] == 0: chain topology
  expect_equal(foc[1, 3], 0)
  expect_equal(foc[3, 1], 0)

  Q <- c(0.5, 0.3, 0.7)
  blockId <- c("A", "A", "A")  # true block: one ownership group of 3

  result <- solveblk(foc, Q, sign = -1, blockId = blockId)
  truth <- as.numeric(-solve(foc, Q))

  expect_equal(result, truth, tolerance = 1e-10)
  ## and the result must actually satisfy the FULL joint FOC system, not just
  ## its own (possibly mis-partitioned) sub-block.
  expect_equal(as.numeric(foc %*% (-result) - Q), c(0, 0, 0), tolerance = 1e-10)
})
