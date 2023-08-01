library(BayesPharma)

testthat::test_that("sigmoid returns numeric class:", {
  testthat::expect_equal(
    class(BayesPharma::sigmoid(-9, -1, 100, 0, -10)),
    "numeric")
})

testthat::test_that("sigmoid returns correct numeric value:", {
  testthat::expect_equal(
    BayesPharma::sigmoid(-9, -1, 100, 0, -10),
    90.90909)
})
