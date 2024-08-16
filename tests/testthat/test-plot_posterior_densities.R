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
        l_ci = 0.025,
        u_ci = 0.975,
        title_label = "Posterior Density Plots with Mean and 95% CI"))
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
