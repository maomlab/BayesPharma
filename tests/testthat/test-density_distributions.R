library(tidymodels)
library(BayesPharma)

load(file = "../testdata/ggplot_test_model.rda")

testthat::test_that("density_distributions is a ggplot object",{
  testthat::expect_s3_class(BayesPharma::density_distributions(test_model),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::density_distributions(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    sample_type = "Prior",
    title = "test_model dens distributions"),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::density_distributions(test_model,
    predictors_col_name = "_Intercept",
    half_max_label = "ic50",
    sample_type = "Posterior",
    title = NULL),
    "ggplot")

})


testthat::test_that("density_distributions throws error if predictors_col_name and
                    half_max_label are not characters",{
  testthat::expect_error(expect_gg(BayesPharma::density_distributions(test_model,
    predictors_col_name = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::density_distributions(test_model,
    predictors_col_name = NULL,
    half_max_label = NULL)))
  testthat::expect_error(expect_gg(BayesPharma::density_distributions(test_model,
    half_max_label = NULL)))

})
