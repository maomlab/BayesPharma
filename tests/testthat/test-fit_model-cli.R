
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
  output_path <-  tempdir()
  data |> readr::write_tsv(file = paste0(output_path, "/data.tsv"))
  
  BayesPharma::fit_model_cli(
    args = c(
      "--data", paste0(output_path, "/data.tsv"),
      "--output_path", output_path,
      "--model", "sigmoid",
      "--formula", "sigmoid_agonist",
      "--verbose"))
  
  parameter_estimates <- readr::read_tsv(
    file =  paste0(output_path, "/parameter_estimates.tsv"),
    show_col_types = FALSE)

  #ec50 is in the 90% credible interval
  testthat::expect_lt(
    parameter_estimates[
      parameter_estimates$variable == "b_ec50_Intercept", "q5"], -6)
  testthat::expect_gt(
    parameter_estimates[
      parameter_estimates$variable == "b_ec50_Intercept", "q95"], -6)

  # hill is in the 90% credible interval"
  testthat::expect_lt(
    parameter_estimates[
      parameter_estimates$variable == "b_hill_Intercept", "q5"], 1)
  testthat::expect_gt(
    parameter_estimates[
      parameter_estimates$variable == "b_hill_Intercept", "q95"], 1)

  # top is in the 90% credible interval"
  testthat::expect_lt(
    parameter_estimates[
      parameter_estimates$variable == "b_top_Intercept", "q5"], 1)
  testthat::expect_gt(
    parameter_estimates[
      parameter_estimates$variable == "b_top_Intercept", "q95"], 1)
  
  # bottom is in the 90% credible interval
  testthat::expect_lt(
    parameter_estimates[
      parameter_estimates$variable == "b_bottom_Intercept", "q5"], 0)
  testthat::expect_gt(
    parameter_estimates[
      parameter_estimates$variable == "b_bottom_Intercept", "q95"], 0)
  
})