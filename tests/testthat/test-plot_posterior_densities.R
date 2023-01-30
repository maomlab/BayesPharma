library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that(
  desc = "default posterior_densities_plot is a ggplot object",
  code = {
    expect_gg(BayesPharma::posterior_densities_plot(test_model))
  })

testthat::test_that(
  desc = "custom posterior_densities_plot is a ggplot object",
  code = {
  expect_gg(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      l_ci = 0.025,
      u_ci = 0.975,
      title_label = "Posterior Density Plots with Mean and 95% CI"))
  })


testthat::test_that(
  desc = "custom posterior_densities_plot with no title is a ggplot object",
  code = {
  expect_gg(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      l_ci = 0.025,
      u_ci = 0.975,
      title_label = NULL))
  })


testthat::test_that(
  desc = paste0(
    "posterior_densities_plot throws error if predictors_col_name is not ",
    "a character"),
  code = {
  testthat::expect_error(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      predictors_col = NULL))
  })

testthat::test_that(
  desc = paste0(
    "posterior_densities_plot throws error if half_max_label is not a ",
    "character"),
  code = {
  testthat::expect_error(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      half_max_label = NULL))
  })

testthat::test_that(
  desc = paste0(
    "posterior_densities_plot throws error if l_ci is not numeric"),
  code = {
  testthat::expect_error(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      u_ci = NULL))
  })

testthat::test_that(
  desc = paste0(
    "posterior_densities_plot throws error if u_ci is not numeric"),
  code = {
  testthat::expect_error(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      u_ci = NULL))
  })
