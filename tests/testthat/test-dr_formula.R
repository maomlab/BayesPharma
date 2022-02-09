library(tidyverse)
library(BayesPharma)

testthat::test_that("dr_formula with a constant predictor", {
  formula <- BayesPharma::dr_formula()

  testthat::expect_true("brmsformula" %in% class(formula))
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

testthat::test_that("dr_formula with a substance predictor", {
  formula <- BayesPharma::dr_formula(
    predictors = substance)

  testthat::expect_true("brmsformula" %in% class(formula))
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

testthat::test_that("dr_formula with a grouped predictor", {
  formula <- BayesPharma::dr_formula(
    predictors = substance + (1 | batch))

  testthat::expect_true("brmsformula" %in% class(formula))
  testthat::expect_equal(formula$resp, "response")
  testthat::expect_equal(
    rlang::f_lhs(formula$formula),
    quote(response))
  testthat::expect_equal(
    rlang::f_rhs(formula$formula),
    quote(sigmoid(ec50, hill, top, bottom, log_dose)))

  testthat::expect_equal(rlang::f_lhs(formula$pforms[[1]]), quote(ec50))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[1]]), quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[2]]), quote(hill))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[2]]), quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[3]]), quote(top))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[3]]), quote(substance + (1 | batch)))
  testthat::expect_equal(rlang::f_lhs(formula$pforms[[4]]), quote(bottom))
  testthat::expect_equal(rlang::f_rhs(formula$pforms[[4]]), quote(substance + (1 | batch)))
})

