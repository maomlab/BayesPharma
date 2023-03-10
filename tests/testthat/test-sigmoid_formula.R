testthat::test_that("sigmoid_agonist_formula with a constant predictor", {
  formula <- BayesPharma::sigmoid_agonist_formula()

  testthat::expect_true(inherits(formula, "brmsformula"))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ec50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ec50))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[1]]), 1L)
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[2]]), 1L)
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[3]]), 1L)
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[4]]), 1L)
})

testthat::test_that("sigmoid_agonist_formula with a substance predictor", {
  formula <- BayesPharma::sigmoid_agonist_formula(
    predictors = substance)

  testthat::expect_true(inherits(formula, "brmsformula"))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ec50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ec50))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[1]]), quote(substance))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[2]]), quote(substance))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[3]]), quote(substance))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[4]]), quote(substance))
})

testthat::test_that("sigmoid_agonist_formula with a grouped predictor", {
  formula <- BayesPharma::sigmoid_agonist_formula(
    predictors = substance + (1 | batch))

  testthat::expect_true(inherits(formula, "brmsformula"))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ec50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ec50))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[1]]),
                         quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[2]]),
                         quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[3]]),
                         quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[4]]),
                         quote(substance + (1 | batch)))
})

##########
testthat::test_that("sigmoid_antagonist_formula with a constant predictor", {
  formula <- BayesPharma::sigmoid_antagonist_formula()

  testthat::expect_true(inherits(formula, "brmsformula"))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ic50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ic50))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[1]]), 1L)
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[2]]), 1L)
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[3]]), 1L)
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[4]]), 1L)
})

testthat::test_that("sigmoid_antagonist_formula with a substance predictor", {
  formula <- BayesPharma::sigmoid_antagonist_formula(
    predictors = substance)

  testthat::expect_true(inherits(formula, "brmsformula"))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ic50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ic50))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[1]]), quote(substance))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[2]]), quote(substance))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[3]]), quote(substance))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[4]]), quote(substance))
})

testthat::test_that("sigmoid_antagonist_formula with a grouped predictor", {
  formula <- BayesPharma::sigmoid_antagonist_formula(
    predictors = substance + (1 | batch))

  testthat::expect_true(inherits(formula, "brmsformula"))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ic50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ic50))
  testthat::expect_equal(
    rlang::f_rhs(formula$pforms[[1]]),
    quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(
    rlang::f_rhs(formula$pforms[[2]]),
    quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(
    rlang::f_rhs(formula$pforms[[3]]),
    quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(
    rlang::f_rhs(formula$pforms[[4]]),
    quote(substance + (1 | batch)))
})
