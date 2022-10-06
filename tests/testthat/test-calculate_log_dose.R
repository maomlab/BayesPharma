library(BayesPharma)
library(tidymodels)

testthat::test_that("adding log dose to data.frame with units M", {
  data <- data.frame(dose = c(1e-6, 1e-7)) |>
    BayesPharma::calculate_log_dose(
      dose_col = dose,
      molar_concentration = 1)
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

testthat::test_that("adding log dose to data.frame using default arguments", {
  data <- data.frame(dose = c(1e-6, 1e-7)) |>
    BayesPharma::calculate_log_dose()
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

testthat::test_that("adding log dose to data.frame with units nM", {
  data <- data.frame(dose_nM = c(1000, 100)) |>
    BayesPharma::calculate_log_dose(
      dose_col = dose_nM,
      molar_concentration = 1e-9)
  testthat::expect_equal(data$log_dose, c(-6, -7))
})
