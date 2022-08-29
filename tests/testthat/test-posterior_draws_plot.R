library(tidymodels)
library(BayesPharma)

load("../testdata/ggplot_test_model.rda")
load("../testdata/ggplot_test_data.rda")

testthat::test_that("posterior_draws_plot returns ggplot object", {
  expect_gg(
    BayesPharma::posterior_draws_plot(
      model = test_model,
      data = test_data,
      facet_var = "test"))

  expect_gg(
    BayesPharma::posterior_draws_plot(
      model = test_model,
      data = test_data,
      predictors_col_name = "test",
      lower = -15,
      upper = -1,
      n = 100,
      facet_var = "XXX",
      title = "Test DR Posterior Draws",
      xlab = "log dose",
      ylab = "sim response"))
})

testthat::test_that(
  paste0(
    "posterior_draws_plot throws error if facet_var is not a name, ",
    "predictors_col_name is not a character vector, and lower, upper and n ",
    "are not numeric"), {

  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        predictors_col_name = NULL)))

  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        lower = NULL)))

  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        upper = "negative three")))

  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        n = NA)))

})
