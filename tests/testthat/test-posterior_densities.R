library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("posterior_densities is a ggplot object",{
  expect_gg(BayesPharma::posterior_densities(test_model))
  expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    l_ci = 0.025,
    u_ci = 0.975,
    title_label = "Posterior Density Plots with Mean and 95% CI"))
  expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    l_ci = 0.05,
    u_ci = 0.95,
    title_label = "Posterior Density Plots with Mean and 90% CI"))
  expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    l_ci = 0.025,
    u_ci = 0.975,
    title_label = NULL))

})


testthat::test_that("posterior_densities throws error if predictors_col_name and
                    half_max_label are not characters",{
  expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = NULL)))
  expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = NULL,
    l_ci = NULL)))
  expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    half_max_label = NULL,
    u_ci = NULL)))
  expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    half_max_label = NULL)))
  expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    l_ci = NULL)))
  expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    u_ci = NULL)))

})


