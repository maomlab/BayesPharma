library(BayesPharma)

testthat::test_that("sigmoid_agonist_prior with default prior", {
  prior <- BayesPharma::sigmoid_agonist_prior()
  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp"  "dpar"  "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
    c("normal(-6, 2.5)", "b", "", "", "", "", "ec50", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[2, ]) ==
    c("normal(1, 1)", "b", "", "", "", "", "hill", NA, "-0.01", "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[3, ]) ==
    c("normal(1, 0.5)", "b", "", "", "", "", "top", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[4, ]) ==
    c("normal(0, 0.5)", "b", "", "", "", "", "bottom", NA, NA, "user"),
    na.rm = TRUE))
})


testthat::test_that("sigmoid_agonist_prior with constant prior", {
  prior <- BayesPharma::sigmoid_agonist_prior(
    ec50 = -7,
    hill = 1.2,
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
      c("constant(1.2)", "b", "", "", "", "", "hill", NA, NA, "user"),
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

testthat::test_that("sigmoid_agonist_prior with custom prior", {
  prior <- BayesPharma::sigmoid_agonist_prior(
    ec50 = brms::prior(prior = student_t(3, -7, 3), nlpar = "ec50", ub = -4),
    hill = brms::prior_string(paste0("constant(-1)"), nlpar = "hill"),
    top = brms::prior(prior = beta(10, 1), nlpar = "top"),
    bottom = brms::prior(prior = beta(1, 10), nlpar = "bottom"))

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

###
testthat::test_that("sigmoid_antagonist_prior with default prior", {
  prior <- BayesPharma::sigmoid_antagonist_prior()
  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp"  "dpar"  "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
      c("normal(-6, 2.5)", "b", "", "", "", "", "ic50", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[2, ]) ==
      c("normal(-1, 1)", "b", "", "", "", "", "hill", NA, "0.01", "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[3, ]) ==
      c("normal(1, 0.5)", "b", "", "", "", "", "top", NA, NA, "user"),
    na.rm = TRUE))

  testthat::expect_true(all(
    as.list(prior[4, ]) ==
      c("normal(0, 0.5)", "b", "", "", "", "", "bottom", NA, NA, "user"),
    na.rm = TRUE))
})


testthat::test_that("sigmoid_antagonist_prior with constant prior", {
  prior <- BayesPharma::sigmoid_antagonist_prior(
    ic50 = -7,
    hill = -1,
    top = 100,
    bottom = 0)

  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp" "dpar" "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
      c("constant(-7)", "b", "", "", "", "", "ic50", NA, NA, "user"),
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

testthat::test_that("sigmoid_antagonist_prior with custom prior", {
  prior <- BayesPharma::sigmoid_antagonist_prior(
    ic50 = brms::prior(prior = student_t(3, -7, 3), nlpar = "ic50", ub = -4),
    hill = brms::prior_string("constant(-1)", nlpar = "hill"),
    top = brms::prior(prior = beta(10, 1), nlpar = "top"),
    bottom = brms::prior(prior = beta(1, 10), nlpar = "bottom"))

  testthat::expect_true("brmsprior" %in% class(prior))

  #"prior" "class" "coef" "group" "resp" "dpar" "nlpar" "lb" "ub" "source"
  testthat::expect_true(all(
    as.list(prior[1, ]) ==
      c("student_t(3, -7, 3)", "b", "", "", "", "", "ic50", NA, -4, "user"),
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
