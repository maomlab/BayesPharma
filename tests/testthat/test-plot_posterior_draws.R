library(tidymodels)
library(BayesPharma)

load("../testdata/ggplot_test_model.rda")
load("../testdata/ggplot_test_data.rda")

testthat::test_that(
  desc = "posterior_draws_plot with facet_var returns ggplot object",
  code = {
  expect_gg(
    BayesPharma::posterior_draws_plot(
      model = test_model,
      data = test_data,
      facet_var = "test"))
})

testthat::test_that(
  desc = "posterior_draws_plot throws error if facet_var is not a name",
  code = {
  expect_gg(
    BayesPharma::posterior_draws_plot(
      model = test_model,
      data = test_data,
      predictors_col = "test",
      lower = -15,
      upper = -1,
      n = 100,
      facet_var = "XXX",
      title = "Test DR Posterior Draws",
      xlab = "log dose",
      ylab = "sim response"))
})

testthat::test_that(
  desc = paste0(
    "posterior_draws_plot throws error ",
    "if predictors_col_name is not a character vector"),
  code = {
  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        predictors_col_name = NULL)))
})

testthat::test_that(
  desc = paste0(
    "posterior_draws_plot throws error ",
    "if lower is not numeric"),
  code = {
    testthat::expect_error(
      expect_gg(
        BayesPharma::posterior_draws_plot(
          model = test_model,
          data = test_data,
          lower = "def")))
  })

testthat::test_that(
  desc = paste0(
    "posterior_draws_plot throws error ",
    "if upper is not numeric"),
  code = {
  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        upper = "negative three")))
})
  
testthat::test_that(
  desc = paste0(
    "posterior_draws_plot throws error ",
    "if n is not numeric"),
  code = {
  testthat::expect_error(
    expect_gg(
      BayesPharma::posterior_draws_plot(
        model = test_model,
        data = test_data,
        n = NA)))
})
