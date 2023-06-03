
testthat::test_that("Fit sigmoid model with cmdstanr", {
  if (!require(cmdstanr)) {
    message(
      "The 'cmdstanr' package is not available, so not running cmdstanr tests")
    return()
  }

  data <- data.frame(
    log_dose = seq(-7, -5, length.out = 30)) |>
    dplyr::mutate(
      response = stats::rnorm(
        n = length(log_dose),
        mean = BayesPharma::sigmoid(
          ac50 = -6,
          hill = 1,
          top = 1,
          bottom = 0,
          log_dose = log_dose),
        sd = .2))

  model <- data |>
    BayesPharma::sigmoid_model(
      formula = BayesPharma::sigmoid_agonist_formula(),
      prior = BayesPharma::sigmoid_agonist_prior(),
      init = BayesPharma::sigmoid_agonist_init(
        sigma = BayesPharma::rstan_default_init(lb = 0)),
      iter = 2000,
      backend = "cmdstanr",
      refresh = 0,
      silent = 2)

  testthat::expect_true(inherits(model, "brmsfit"))

  n_divergences <- model |>
    brms::nuts_params() |>
    dplyr::filter(Parameter == "divergent__", Value > 0) |>
    nrow()
  testthat::expect_lt(n_divergences, 100)
})
