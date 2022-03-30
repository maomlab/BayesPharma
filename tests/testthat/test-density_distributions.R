library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("traceplot is a ggplot object",{
  expect_gg(BayesPharma::density_distributions(test_model))
  expect_gg(BayesPharma::density_distributions(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    sample_type = "Prior",
    title = "test_model dens distributions"))
  expect_gg(BayesPharma::density_distributions(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    sample_type = "Posterior",
    title = NULL))

})


testthat::test_that("traceplot throws error if predictors_col_name and
                    half_max_label are not characters",{
  expect_error(expect_gg(BayesPharma::density_distributions(test_model,
    predictors_col_name = NULL)))
  expect_error(expect_gg(BayesPharma::density_distributions(test_model,
    predictors_col_name = NULL,
    half_max_label = NULL)))
  expect_error(expect_gg(BayesPharma::density_distributions(test_model,
    half_max_label = NULL)))

})


