library(tidymodels)
library(BayesPharma)

load("../testdata/ggplot_test_model.rda")

testthat::test_that("posterior_draws_plot returns ggplot object",{
  testthat::expect_s3_class(BayesPharma::posterior_draws_plot(test_model,
    data = test_data, facet_var = "test"),
    "ggplot")
  testthat::expect_s3_class(BayesPharma::posterior_draws_plot(test_model,
    data = test_data, predictors_col_name = "test", lower = -15, upper = -1,
    n = 100, facet_var = "XXX", title = "Test DR Posterior Draws",
    xlab = "log dose", ylab = "sim response"),
    "ggplot")
})

testthat::test_that("posterior_draws_plot throws error if facet_var is not a
                    name, predictors_col_name is not a character vector, and
                    lower, upper and n are not numeric",{
  testthat::expect_error(BayesPharma::posterior_draws_plot(test_model,
    data = test_data, predictors_col_name = NULL))
  testthat::expect_error(BayesPharma::posterior_draws_plot(test_model,
    data = test_data, lower = NULL))
  testthat::expect_error(BayesPharma::posterior_draws_plot(test_model,
    data = test_data, upper = "negative three"))
  testthat::expect_error(BayesPharma::posterior_draws_plot(test_model,
    data = test_data, n = NA))

})
