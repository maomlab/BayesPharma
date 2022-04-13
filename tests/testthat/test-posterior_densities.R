library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("posterior_densities is a ggplot object", {
  testthat::expect_s3_class(BayesPharma::posterior_densities(test_model),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    l_ci = 0.025,
    u_ci = 0.975,
    title_label = "Posterior Density Plots with Mean and 95% CI"),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    l_ci = 0.05,
    u_ci = 0.95,
    title_label = "Posterior Density Plots with Mean and 90% CI"),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    l_ci = 0.025,
    u_ci = 0.975,
    title_label = NULL),
    "ggplot")

})


testthat::test_that("posterior_densities throws error if predictors_col_name and
                    half_max_label are not characters, and l_ci and u_ci are
                    not numeric", {
  testthat::expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    predictors_col_name = NULL,
    l_ci = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    half_max_label = NULL,
    u_ci = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    half_max_label = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    l_ci = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::posterior_densities(test_model,
    u_ci = NULL)))

})
