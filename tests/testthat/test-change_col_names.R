library(BayesPharma)
library(tidymodels)

testthat::test_that("changing column names of a data.frame", {
  data <- data.frame(measurement = c(100, 50, 0), log10_dose = c(-12, -9, -6),
                     perturbation = c(rep("drug_A",3))) %>%
    BayesPharma::change_col_names(response_col_name = measurement,
                                  log_dose_col_name = log10_dose,
                                  predictors_col_name = perturbation)
  testthat::expect_equal(colnames(data), c("response", "log_dose", "predictors"))
})

testthat::test_that("changing column names of a data.frame using default arguments", {
  data <- data.frame(measurement = c(100, 50, 0), log10_dose = c(-12, -9, -6),
                     perturbation = c(rep("drug_A",3))) %>%
    BayesPharma::change_col_names()
  testthat::expect_equal(colnames(data), c("measurement", "log10_dose", "perturbation"))
})
