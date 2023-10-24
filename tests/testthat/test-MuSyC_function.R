library(BayesPharma)

#' let
#'   treatment 1 have parameters
#'      bottom: 0    => logE0 = log(0)
#'      top: 1       => logE1 = log(1)
#'      EC50: 1 nM   => logC1 = log(1e-9 / 1e-6)
#'      hill: 1      => si = 1 => hi = 4
#'  treatment 2 have parameters
#'      bottom: 0    => logE0 = log(0)
#'      top: 1       => logE1 = log(1)
#'      EC50: 10 nM  => logC1 = log(1e-9 / 1e-6)
#'      hill: 2      => si = 1 => hi = 8
#'   synergistic potency
#'     logalpha: log(5)
#'   synergistic efficacy
#'     logE3: 1
#'
#' note the bottom parameter for treatment 1 and treatment 2 are shared
#'
#' Let the logd1scale and logd2scale to be micromolar e.g. 1e-6
#' then logd1, logd2, logC1, and logC2 all scaled accordingly
#'
#' test at
#'   treatment 1: 1 nM => logd1 = log(1e-9 / 1e-6)
#'   treatment 2: 1 uM => logd2 = log(1e-6 / 1e-6)
#'
testthat::test_that("MuSyC returns numeric class:", {
  testthat::expect_equal(
    BayesPharma::MuSyC(
      logd1 = log(1e-9 / 1e-6),
      logd2 = log(1),
      logE0 = log(0),
      logE1 = log(1),
      logC1 = log(0.01),
      h1 = BayesPharma::MuSyC_si_to_hi(
        si = 1, Ci = 0.01, E0 = 0, Ei = 1),
      logE2 = log(1),
      logC2 = log(1),
      h2 = BayesPharma::MuSyC_si_to_hi(
        si = 2, Ci = 1, E0 = 0, Ei = 1),
      logE3 = log(1),
      logalpha = log(5)) |> class(),
    "numeric")
})

testthat::test_that("MuSyC returns correct numeric value:", {
  testthat::expect_equal(
    BayesPharma::MuSyC(
      logd1 = log(0.001),
      logd2 = log(1),
      logE0 = log(0),
      logE1 = log(1),
      logC1 = log(0.01),
      h1 = BayesPharma::MuSyC_si_to_hi(
        si = 1, Ci = 0.01, E0 = 0, Ei = 1),
      logE2 = log(1),
      logC2 = log(1),
      h2 = BayesPharma::MuSyC_si_to_hi(
        si = 2, Ci = 1, E0 = 0, Ei = 1),
      logE3 = log(1),
      logalpha = log(5)),
    0.8661682)
})

testthat::test_that("MuSyC returns correct numeric value at zero doses:", {
  testthat::expect_equal(
    BayesPharma::MuSyC(
      logd1 = log(0),
      logd2 = log(0),
      logE0 = log(0),
      logE1 = log(1),
      logC1 = log(0.01),
      h1 = BayesPharma::MuSyC_si_to_hi(
        si = 1, Ci = 0.01, E0 = 0, Ei = 1),
      logE2 = log(1),
      logC2 = log(1),
      h2 = BayesPharma::MuSyC_si_to_hi(
        si = 2, Ci = 1, E0 = 0, Ei = 1),
      logE3 = log(1),
      logalpha = log(5)),
    0)
})


testthat::test_that("MuSyC function is defined in inhibition mode", {
  testthat::expect_equal(
    BayesPharma::MuSyC(
      logd1 = log(0.001),
      logd2 = log(1),
      logE0 = log(1),
      logE1 = log(0),
      logC1 = log(0.01),
      h1 = BayesPharma::MuSyC_si_to_hi(
        si = -1, Ci = 0.01, E0 = 0, Ei = 1),
      logE2 = log(0),
      logC2 = log(1),
      h2 = BayesPharma::MuSyC_si_to_hi(
        si = -2, Ci = 1, E0 = 0, Ei = 1),
      logE3 = log(0),
      logalpha = log(5)),
    0.1165655,
    tolerance = 1e-4)
})

testthat::test_that("MuSyC function is defined in inhibition mode", {
  testthat::expect_equal(
    BayesPharma::MuSyC(
      logd1 = log(0),
      logd2 = log(0),
      logE0 = log(1),
      logE1 = log(0),
      logC1 = log(0.01),
      h1 = BayesPharma::MuSyC_si_to_hi(
        si = -1, Ci = 0.01, E0 = 0, Ei = 1),
      logE2 = log(0),
      logC2 = log(1),
      h2 = BayesPharma::MuSyC_si_to_hi(
        si = -2, Ci = 1, E0 = 0, Ei = 1),
      logE3 = log(0),
      logalpha = log(5)),
    1)
})


testthat::test_that("R and stan versions of MuSyC agree", {

  model <- brms::brm(
    formula = y ~ 1,
    data = data.frame(y = rep(1, 10)),
    stanvars = BayesPharma::MuSyC_stanvar(),
    chains = 0)
  model |> brms::expose_functions(vectorize = TRUE)

  z <- tidyr::expand_grid(
    logd1 = log(c(0, 1)),
    logd2 = log(c(0, 1)),
    logE0 = log(c(0, 1)),
    logE1 = log(c(0, 0)),
    logC1 = log(c(1e-9, 1)),
    h1 = c(-4, 4),
    logE2 = log(c(0, 0)),
    logC2 = log(c(1e-9, 1)),
    h2 = c(-4, 4),
    logE3 = log(c(0, 1)),
    logalpha = log(c(.1, 10))) |>
    dplyr::rowwise() |>
    dplyr::do({
      params <- .
      testthat::expect_equal(
        MuSyC(
          logd1 = params$logd1[1],
          logd2 = params$logd2[1],
          logE0 = params$logE0[1],
          logC1 = params$logC1[1],
          logE1 = params$logE1[1],
          h1 = params$h1[1],
          logC2 = params$logC2[1],
          logE2 = params$logE2[1],
          h2 = params$h2[1],
          logE3 = params$logE3[1],
          logalpha = params$logalpha[1]),
        BayesPharma::MuSyC(
          logd1 = params$logd1[1],
          logd2 = params$logd2[1],
          logE0 = params$logE0[1],
          logC1 = params$logC1[1],
          logE1 = params$logE1[1],
          h1 = params$h1[1],
          logC2 = params$logC2[1],
          logE2 = params$logE2[1],
          h2 = params$h2[1],
          logE3 = params$logE3[1],
          logalpha = params$logalpha[1]))
      data.frame()
    })
})
