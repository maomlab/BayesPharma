library(BayesPharma)
library(tidymodels)

testthat::test_that(
  desc = "sigmoid_agoinst_inits with default inits",
  code = {
    init <- sigmoid_agonist_init()
    testthat::expect_true(methods::is(init, "list"))
  })

testthat::test_that(
  desc = "sigmoid_agoinst_inits with custom inits",
  code = {
  init <- sigmoid_agonist_init(
    ec50 = -6,
    hill = 0.5,
    top = 50,
    bottom = 0)

  testthat::expect_true(methods::is(init, "list"))
})

testthat::test_that(
  desc = "sigmoid_antagonist_inits with default inits",
  code = {
    init <- sigmoid_antagoinst_init()
    testthat::expect_true(methods::is(init, "list"))
  })

testthat::test_that(
  desc = "sigmoid_antagonist_inits with custom inits",
  code = {
    init <- sigmoid_antagonist_init(
      ic50 = -6,
      hill = 0.5,
      top = 50,
      bottom = 0)
    
    testthat::expect_true(methods::is(init, "list"))
  })