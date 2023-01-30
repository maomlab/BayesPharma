testthat::test_that("tQ_model", {
  data_single <- BayesPharma::tQ_model_generate(
    time = seq(0.1, 3, by=.05),
    kcat = 3,
    kM = 5,
    ET = 10,
    ST = 10) |>
    as.data.frame() |>
    dplyr::rename(P_true = 2) |>
    dplyr::mutate(
      series_index = 1,
      P = rnorm(dplyr::n(), P_true, 0.5), # add some observational noise
      ST = 10, ET = 10)
  
  model <- data_single |>
    BayesPharma::tQ_model()

  n_divergences <- model |>
    brms::nuts_params() |>
    dplyr::filter(Parameter == "divergent__", Value > 0) |>
    nrow()
  testthat::expect_lt(n_divergences, 100)
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
  testthat::expect_true(inherits(formula, "brmsfit"))
  
  n_divergences <- model |>
    brms::nuts_params() |>
    dplyr::filter(Parameter == "divergent__", Value > 0) |>
    nrow()
  testthat::expect_lt(n_divergences, 100)
})
