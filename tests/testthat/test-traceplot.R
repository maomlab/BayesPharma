library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("traceplot is a ggplot object", {

  expect_gg(
    BayesPharma::traceplot(model = test_model))

  expect_gg(
    BayesPharma::traceplot(
      model = test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      title = "test_model traceplot"))

  expect_gg(
    BayesPharma::traceplot(
      model = test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      title = NULL))

})


testthat::test_that(
  paste0(
    "traceplot throws error if predictors_col_name and half_max_label are not ",
    "characters"), {

  testthat::expect_error(
    expect_gg(
      BayesPharma::traceplot(
        model = test_model,
        predictors_col_name = NULL)))

  testthat::expect_error(
    expect_gg(
      BayesPharma::traceplot(
        model = test_model,
        predictors_col_name = NULL,
        half_max_label = NULL)))

  testthat::expect_error(
    expect_gg(
      BayesPharma::traceplot(
        model = test_model,
        half_max_label = NULL)))

})
