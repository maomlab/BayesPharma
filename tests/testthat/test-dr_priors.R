library(tidyverse)
library(BayesPharma)


testthat::test_that("dr_priors with default priors", {
  priors <- BayesPharma::dr_priors()

  testthat::expect_true("brmsprior" %in% class(priors))

  #"prior"  "class"  "coef"   "group"  "resp"   "dpar"   "nlpar"  "bound"  "source"
  testthat::expect_true(all(
    as.list(priors[1,]) ==
    c("normal(-7, 2.5)", "b", "", "", "", "", "ec50", "", "user")))

  testthat::expect_true(all(
    as.list(priors[2,]) ==
    c("normal(-1, 1)", "b", "", "", "", "", "hill", "<upper=0.01>", "user")))

  testthat::expect_true(all(
    as.list(priors[3,]) ==
    c("normal(100, 25)", "b", "", "", "", "", "top", "", "user")))

  testthat::expect_true(all(
    as.list(priors[4,]) ==
    c("normal(0, 25)", "b", "", "", "", "", "bottom", "", "user")))
})


testthat::test_that("dr_priors with constant priors", {
  priors <- BayesPharma::dr_priors(
    ec50 = -7,
    hill = -1,
    top = 100,
    bottom = 0)

  testthat::expect_true("brmsprior" %in% class(priors))

  #"prior"  "class"  "coef"   "group"  "resp"   "dpar"   "nlpar"  "bound"  "source"
  testthat::expect_true(all(
    as.list(priors[1,]) ==
      c("constant(-7)", "b", "", "", "", "", "ec50", "", "user")))

  testthat::expect_true(all(
    as.list(priors[2,]) ==
      c("constant(-1)", "b", "", "", "", "", "hill", "", "user")))

  testthat::expect_true(all(
    as.list(priors[3,]) ==
      c("constant(100)", "b", "", "", "", "", "top", "", "user")))

  testthat::expect_true(all(
    as.list(priors[4,]) ==
      c("constant(0)", "b", "", "", "", "", "bottom", "", "user")))
})

testthat::test_that("dr_priors with custom priors", {
  priors <- BayesPharma::dr_priors(
    ec50 = brms::prior(student_t(3, -7, 3), nlpar = "ec50", ub = -4),
    hill = brms::prior_string(paste0("constant(-1)"), nlpar = "hill"),
    top = brms::prior(beta(10, 1), nlpar = "top"),
    bottom = brms::prior(beta(1, 10), nlpar = "bottom"))

  testthat::expect_true("brmsprior" %in% class(priors))

  #"prior"  "class"  "coef"   "group"  "resp"   "dpar"   "nlpar"  "bound"  "source"
  testthat::expect_true(all(
    as.list(priors[1,]) ==
      c("student_t(3, -7, 3)", "b", "", "", "", "", "ec50", "<upper=-4>", "user")))

  testthat::expect_true(all(
    as.list(priors[2,]) ==
      c("constant(-1)", "b", "", "", "", "", "hill", "", "user")))

  testthat::expect_true(all(
    as.list(priors[3,]) ==
      c("beta(10, 1)", "b", "", "", "", "", "top", "", "user")))

  testthat::expect_true(all(
    as.list(priors[4,]) ==
      c("beta(1, 10)", "b", "", "", "", "", "bottom", "", "user")))
})

testthat::test_that("dr_priors in agonist mode", {
  priors <- BayesPharma::dr_priors(inhibitor=FALSE)
  testthat::expect_true("brmsprior" %in% class(priors))

  testthat::expect_true(all(
    as.list(priors[2,]) ==
      c("normal(1, 1)", "b", "", "", "", "", "hill", "<lower=-0.01>", "user")))

})

