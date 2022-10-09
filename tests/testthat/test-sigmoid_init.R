library(BayesPharma)
library(tidymodels)

testthat::test_that(
  desc = "sigmoid_inits with default inits",
  code = {
  init <- sigmoid_init()

  testthat::expect_true("list" %in% class(init))
  testthat::expect_equal(
    init,
    rep(list(list(ec50 = -9, hill = -1, top = 100, bottom = 0)), 4))

})

testthat::test_that(
  desc = "sigmoid_inits with custom inits",
  code = {
  init <- sigmoid_init(
    ec50 = -6,
    hill = 0.5,
    top = 50,
    bottom = 0,
    chains = 2)

  testthat::expect_true("list" %in% class(init))
  testthat::expect_equal(
    init,
    rep(list(list(ec50 = -6, hill = 0.5, top = 50, bottom = 0)), 2))

})
