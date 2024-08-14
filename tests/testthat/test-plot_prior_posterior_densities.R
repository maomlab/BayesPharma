library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("plot_prior_posterior_densities is a ggplot object", {
  expect_gg(
    BayesPharma::plot_prior_posterior_densities(test_model))

  expect_gg(
    BayesPharma::plot_prior_posterior_densities(
      model = test_model,
      title_label = "Prior Posterior Density Overlay Plots"))

  expect_gg(
    BayesPharma::plot_prior_posterior_densities(
      model = test_model,
      title_label = NULL))
})

