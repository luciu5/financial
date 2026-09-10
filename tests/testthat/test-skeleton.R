test_that("package loads and FinancialFit is a virtual class", {
  expect_true(isVirtualClass("FinancialFit"))
  expect_true(extends("FinancialFit", "StructuralFit"))
})
