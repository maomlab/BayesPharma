library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that(
  desc = "simple density_distributions is a ggplot object",
  code = {
  expect_gg(
    BayesPharma::density_distributions_plot(test_model))
  })

testthat::test_that(
  desc = "custom density_distributions is a ggplot object",
  code = {
  expect_gg(
    BayesPharma::density_distributions_plot(
      test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      sample_type = "Prior",
      title = "test_model dens distributions"))
  })

testthat::test_that(
  desc = "alt custom density_distributions is a ggplot object",
  code = {
  expect_gg(
    BayesPharma::density_distributions_plot(
      test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      sample_type = "Posterior",
      title = NULL))
  })


testthat::test_that(
  desc = paste0(
    "density_distributions throws error if predictors_col_name is not
    a character"),
  code = {
  testthat::expect_error(
    expect_gg(
      BayesPharma::density_distributions_plot(
        test_model,
        predictors_col_name = NULL)))
  })

testthat::test_that(
  desc = paste0(
    "density_distributions throws error if half_max_label is not a character"),
  code = {
  testthat::expect_error(
    expect_gg(
      BayesPharma::density_distributions_plot(
      test_model,
      half_max_label = NULL)))
  })
