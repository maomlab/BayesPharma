library(BayesPharma)
library(tidymodels)

testthat::test_that("dr_inits with default inits", {
  inits <- dr_inits()

  testthat::expect_true("list" %in% class(inits))
  testthat::expect_equal(
    inits,
    rep(list(list(ec50 = -9, hill = -1, top = 100, bottom = 0)), 4))

})

testthat::test_that(
  "dr_inits with custom inits", {
  inits <- dr_inits(
    ec50 = -6,
    hill = 0.5,
    top = 50,
    bottom = 0,
    chains = 2)

  testthat::expect_true("list" %in% class(inits))
  testthat::expect_equal(
    inits,
    rep(list(list(ec50 = -6, hill = 0.5, top = 50, bottom = 0)), 2))

})
