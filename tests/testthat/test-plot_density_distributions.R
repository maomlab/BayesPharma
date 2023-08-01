library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that(
  desc = "simple density distribution is a ggplot object",
  code = {
    expect_gg(
      BayesPharma::plot_density_distribution(test_model))
  })


testthat::test_that(
  desc = "simple density distribution with custom pars is a ggplot object",
  code = {
    expect_gg(
      BayesPharma::plot_density_distribution(
        model = test_model,
        pars = c("b_ec50_Intercept")))
  })

testthat::test_that(
  desc = "simple density distributions with custom labeller is a ggplot object",
  code = {
    expect_gg(
      BayesPharma::plot_density_distribution(
        model = test_model,
        pars = c("b_ec50_Intercept"),
        labeller = ggplot2::labeller(ec50 = "EC50")))
  })
