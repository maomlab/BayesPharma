library(tidymodels)
library(BayesPharma)

load("../testdata/ggplot_test_model.rda")

testthat::test_that(
  desc = "posterior_draws_plot with facet_var returns ggplot object",
  code = {
  expect_gg(
    BayesPharma::posterior_draws_plot(
      model = test_model))
})
