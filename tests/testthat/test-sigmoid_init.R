library(BayesPharma)

testthat::test_that(
  desc = "sigmoid_agonist_inits with default inits",
  code = {
    init <- sigmoid_agonist_init()
    testthat::expect_true(methods::is(init, "function"))
  })

testthat::test_that(
  desc = "sigmoid_agonist_inits with custom inits",
  code = {
  init <- sigmoid_agonist_init(
    ec50 = -6,
    hill = 0.5,
    top = 50,
    bottom = 0)

  testthat::expect_true(methods::is(init, "function"))
})

testthat::test_that(
  desc = "sigmoid_antagonist_inits with default inits",
  code = {
    init <- sigmoid_antagonist_init()
    testthat::expect_true(methods::is(init, "function"))
  })

testthat::test_that(
  desc = "sigmoid_antagonist_inits with custom inits",
  code = {
    init <- sigmoid_antagonist_init(
      ic50 = -6,
      hill = 0.5,
      top = 50,
      bottom = 0)

    testthat::expect_true(methods::is(init, "function"))
  })
