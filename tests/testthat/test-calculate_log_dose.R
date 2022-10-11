library(BayesPharma)
library(tidymodels)

testthat::test_that(
  desc = "adding log dose to data.frame using default arguments",
  code = {
    data <- data.frame(dose = c(1e-6, 1e-7)) |>
      BayesPharma::calculate_log_dose()
    testthat::expect_equal(data$log_dose, c(-6, -7))
  })

testthat::test_that(
  desc = "adding log dose to data.frame using symbol dose_col",
  code = {
  data <- data.frame(dose = c(1e-6, 1e-7)) |>
    BayesPharma::calculate_log_dose(
      dose_col = dose)
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

testthat::test_that(
  desc = "adding log dose to data.frame with string dose_col",
  code = {
  data <- data.frame(dose = c(1e-6, 1e-7)) |>
    BayesPharma::calculate_log_dose(
      dose_col = "dose")
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

testthat::test_that(
  desc = "adding log dose to data.frame with numeric dose_col",
  code = {
  data <- data.frame(id = c(1, 2), dose = c(1e-6, 1e-7)) |>
    BayesPharma::calculate_log_dose(
      dose_col = 2)
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

testthat::test_that(
  desc = "adding log dose to data.frame with units M using expression",
  code = {
  data <- data.frame(dose = c(1e-6, 1e-7)) |>
    BayesPharma::calculate_log_dose(
      dose_col = dose,
      molar_concentration = 1)
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

testthat::test_that(
  desc = "adding log dose to data.frame with units nM",
  code = {
  data <- data.frame(dose_nM = c(1000, 100)) |>
    BayesPharma::calculate_log_dose(
      dose_col = dose_nM,
      molar_concentration = 1e-9)
  testthat::expect_equal(data$log_dose, c(-6, -7))
})

########

testthat::test_that(
  desc = "Error if the default agument is a colum in data",
  code = {
  testthat::expect_error(
    data <- data.frame(dose_nM = c(1000, 100)) |>
      BayesPharma::calculate_log_dose(
        molar_concentration = 1e-9))
})

testthat::test_that(
  desc = "Error if specifying a column that is not in data",
  code = {
  testthat::expect_error(
    data <- data.frame(dose_nM = c(1000, 100)) |>
      BayesPharma::calculate_log_dose(
        dose_col = dose,
        molar_concentration = 1e-9))
})

testthat::test_that(
  desc = "Error if specifying a string column that is not in data",
  code = {
  testthat::expect_error(
    data <- data.frame(dose_nM = c(1000, 100)) |>
      BayesPharma::calculate_log_dose(
        dose_col = "dose",
        molar_concentration = 1e-9))
})

testthat::test_that(
  desc = "Error if specifying a numeric column that is not in data",
  code = {
  testthat::expect_error(
    data <- data.frame(dose_nM = c(1000, 100)) |>
      BayesPharma::calculate_log_dose(
        dose_col = 10,
        molar_concentration = 1e-9))
})
