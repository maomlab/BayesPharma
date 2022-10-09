library(BayesPharma)
library(tidymodels)


testthat::test_that("sigmoid_prior with default prior", {
  prior <- BayesPharma::sigmoid_prior()

  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp"  "dpar"  "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
    c("normal(-7, 2.5)", "b", "", "", "", "", "ec50", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[2, ]) ==
    c("normal(-1, 1)", "b", "", "", "", "", "hill", NA, "0.01", "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[3, ]) ==
    c("normal(100, 25)", "b", "", "", "", "", "top", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[4, ]) ==
    c("normal(0, 25)", "b", "", "", "", "", "bottom", NA, NA, "user"),
    na.rm = TRUE))
})


testthat::test_that("sigmoid_prior with constant prior", {
  prior <- BayesPharma::sigmoid_prior(
    ec50 = -7,
    hill = -1,
    top = 100,
    bottom = 0)

  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp" "dpar" "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
      c("constant(-7)", "b", "", "", "", "", "ec50", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[2, ]) ==
      c("constant(-1)", "b", "", "", "", "", "hill", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[3, ]) ==
      c("constant(100)", "b", "", "", "", "", "top", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[4, ]) ==
      c("constant(0)", "b", "", "", "", "", "bottom", NA, NA, "user"),
    na.rm = TRUE))
})

testthat::test_that("sigmoid_prior with custom prior", {
  prior <- BayesPharma::sigmoid_prior(
    ec50 = brms::prior(student_t(3, -7, 3), nlpar = "ec50", ub = -4),
    hill = brms::prior_string(paste0("constant(-1)"), nlpar = "hill"),
    top = brms::prior(beta(10, 1), nlpar = "top"),
    bottom = brms::prior(beta(1, 10), nlpar = "bottom"))

  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp" "dpar" "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
      c("student_t(3, -7, 3)", "b", "", "", "", "", "ec50", NA, -4, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[2, ]) ==
      c("constant(-1)", "b", "", "", "", "", "hill", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[3, ]) ==
      c("beta(10, 1)", "b", "", "", "", "", "top", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[4, ]) ==
      c("beta(1, 10)", "b", "", "", "", "", "bottom", NA, NA, "user"),
    na.rm = TRUE))
})

testthat::test_that("sigmoid_prior in agonist mode", {
  prior <- BayesPharma::sigmoid_prior(inhibitor = FALSE)
  testthat::expect_true("brmsprior" %in% class(prior))

  testthat::expect_true(all(
    as.list(prior[2, ]) ==
      c("normal(1, 1)", "b", "", "", "", "", "hill", -0.01, NA, "user"),
    na.rm = TRUE))

})