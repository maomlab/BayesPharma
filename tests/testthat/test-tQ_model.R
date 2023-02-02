testthat::test_that("tQ_model", {
  data_single <- BayesPharma::tQ_model_generate(
    time = seq(0.1, 3, by = .5),
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
