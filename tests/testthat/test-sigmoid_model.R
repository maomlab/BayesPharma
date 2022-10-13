
testthat::test_that("Agonist sigmoid model fit with zero doses", {
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
        sd = .2)) |>
    dplyr::bind_rows(
      data.frame(log_dose = -Inf, response = 0))
  
  model <- data |>
    BayesPharma::sigmoid_agonist_model(
      iter = 2000)
  testthat::expect_equal(class(model), "brmsfit")
  
  n_divergences <- model |>
    brms::nuts_params() |>
    dplyr::filter(Parameter == "divergent__", Value > 0) |>
    nrow()
  testthat::expect_less_than(n_divergences, 100)
})



testthat::test_that("Antagonist sigmoid model fit with zero doses", {
  data <- data.frame(
    log_dose = seq(-7, -5, length.out = 30)) |>
    dplyr::mutate(
      response = stats::rnorm(
        n = length(log_dose),
        mean = BayesPharma::sigmoid(
          ac50 = -6,
          hill = -1,
          top = 1,
          bottom = 0,
          log_dose = log_dose),
        sd = .2)) |>
    dplyr::bind_rows(
      data.frame(log_dose = -Inf, response = 0))

  model <- data |>
    BayesPharma::sigmoid_antagonist_model(
      iter = 2000)
  testthat::expect_equal(class(model), "brmsfit")
  
  n_divergences <- model |>
    brms::nuts_params() |>
    dplyr::filter(Parameter == "divergent__", Value > 0) |>
    nrow()
  testthat::expect_less_than(n_divergences, 100)
})

