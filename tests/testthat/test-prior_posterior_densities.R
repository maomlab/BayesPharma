library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("prior_posterior_densities is a ggplot object", {
  testthat::expect_s3_class(BayesPharma::prior_posterior_densities(test_model),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::prior_posterior_densities(test_model,
     predictors_col_name = "_Intercept",
     half_max_label = "ic50",
     title_label = "Prior Posterior Density Overlay Plots"),
     "ggplot")
  testthat::expect_s3_class(BayesPharma::prior_posterior_densities(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    title_label = NULL),
    "ggplot")

})

testthat::test_that("prior_posterior_densities throws error if
                     predictors_col_name and half_max_label are
                     not characters", {
  testthat::expect_error(expect_gg(BayesPharma::prior_posterior_densities(
    test_model,
    predictors_col_name = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::prior_posterior_densities(
    test_model,
    predictors_col_name = NULL,
    half_max_label = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::prior_posterior_densities(
    test_model,
    half_max_label = NULL)))

})
