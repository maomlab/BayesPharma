# library(tidyverse)
# library(BayesPharma)

# testthat::test_that("Sigmoid model fit with ideal data", {
#   sigmoid <- function(ec50, hill, top, bottom, log_dose) {
#     # simple version that doesn't handle 0-doses
#     bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))
#   }
#   data <- data.frame(
#     log_dose = seq(-7, -5, length.out = 30)) %>%
#     dplyr::mutate(
#       response = log_dose %>%
#         sigmoid(ec50 = -6, hill = 1, top = 1, bottom = 0, log_dose = .) %>%
#         rnorm(sd = .2))
#
#   model <- data %>%
#     BayesPharma::dr_model(
#       formula = BayesPharma::dr_formula(),
#       priors = BayesPharma::dr_priors())
# })
#
# testthat::test_that("Sigmoid model fit with zero doses", {
#   sigmoid <- function(ec50, hill, top, bottom, log_dose) {
#     # simple version that doesn't handle 0-doses
#     bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))
#   }
#   data <- data.frame(
#     log_dose = seq(-7, -5, length.out = 30)) %>%
#     dplyr::mutate(
#       response = log_dose %>%
#         sigmoid(ec50 = -6, hill = 1, top = 1, bottom = 0, log_dose = .) %>%
#         rnorm(sd = 0.2)) %>%
#     dplyr::bind_rows(
#       data.frame(log_dose = -Inf, response = 0))
#
#   model <- data %>%
#     BayesPharma::dr_model(
#       formula = BayesPharma::dr_formula(),
#       priors = BayesPharma::dr_priors())
#})
