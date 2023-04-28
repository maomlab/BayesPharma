library(BayesPharma)

load("../testdata/ggplot_test_model.rda")

testthat::test_that(
  desc = "plot_posterior_draws with facet_var returns ggplot object",
  code = {
  test_model |> brms::expose_functions(test_model, vectorize = TRUE)
  expect_gg(
    BayesPharma::plot_posterior_draws(
      model = test_model))
})
