library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that(
  desc = "Result of default plot_posterior_density is a ggplot2::ggplot object",
  code = {
    expect_gg(BayesPharma::plot_posterior_density(test_model))
  })

testthat::test_that(
  desc = "Result of custom plot_posterior_density is a ggplot2::ggplot object",
  code = {
  expect_gg(
    BayesPharma::plot_posterior_density(
      model = test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      l_ci = 0.025,
      u_ci = 0.975,
      title_label = "Posterior Density Plots with Mean and 95% CI"))
  })


testthat::test_that(
  desc =
    "Result of custom plot_posterior_density with no title is a ggplot object",
  code = {
  expect_gg(
    BayesPharma::plot_posterior_density(
      model = test_model,
      predictors_col_name = "_Intercept",
      half_max_label = "ic50",
      l_ci = 0.025,
      u_ci = 0.975,
      title_label = NULL))
  })


testthat::test_that(
  desc = paste0(
    "plot_posterior_density throws error if predictors_col_name is not ",
    "a character"),
  code = {
  testthat::expect_error(
    BayesPharma::plot_posterior_density(
      model = test_model,
      predictors_col = NULL))
  })

testthat::test_that(
  desc = paste0(
    "plot_posterior_density throws error if half_max_label is not a ",
    "character"),
  code = {
  testthat::expect_error(
    BayesPharma::plot_posterior_density(
      model = test_model,
      half_max_label = NULL))
  })

testthat::test_that(
  desc = paste0(
    "plot_posterior_density throws error if l_ci is not numeric"),
  code = {
  testthat::expect_error(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      u_ci = NULL))
  })

testthat::test_that(
  desc = paste0(
    "plot_posterior_density throws error if u_ci is not numeric"),
  code = {
  testthat::expect_error(
    BayesPharma::posterior_densities_plot(
      model = test_model,
      u_ci = NULL))
  })
