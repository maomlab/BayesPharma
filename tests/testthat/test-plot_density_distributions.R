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
  desc = "simple density_distributions with custom pars is a ggplot object",
  code = {
    expect_gg(
      BayesPharma::density_distributions_plot(
        model = test_model,
        pars = c("b_ec50_Intercept")))
  })

testthat::test_that(
  desc = "simple density_distributions with custom labeller is a ggplot object",
  code = {
    expect_gg(
      BayesPharma::density_distributions_plot(
        model = test_model,
        pars = c("b_ec50_Intercept"),
        labeller = ggplot2::labeller(ec50 = "EC50")))
  })
