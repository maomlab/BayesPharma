
testthat::test_that("Sigmoid model fit with ideal data", {
  data <- data.frame(
    log_dose = seq(-7, -5, length.out = 30)) |>
    dplyr::mutate(
      response = rnorm(
        n = length(log_dose), 
        mean = BayesPharma::sigmoid(
          ec50 = -6,
          hill = 1,
          top = 1,
          bottom = 0,
          log_dose = log_dose),
        sd = .2))

  model <- data |>
    BayesPharma::sigmoid_model(
      iter = 2000)
})

testthat::test_that("Sigmoid model fit with zero doses", {
  data <- data.frame(
    log_dose = seq(-7, -5, length.out = 30)) |>
    dplyr::mutate(
      response = rnorm(
        n = length(log_dose), 
        mean = BayesPharma::sigmoid(
          ec50 = -6,
          hill = 1,
          top = 1,
          bottom = 0,
          log_dose = log_dose),
        sd = .2)) |>
    dplyr::bind_rows(
      data.frame(log_dose = -Inf, response = 0))

  model <- data |>
    BayesPharma::sigmoid_model()
})
