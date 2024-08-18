#' Fit a MuSyC Synergy Model
#'
#' @description The MuSyC synergy model is a bivariate functional form with
#'   Bliss and Loewe synergy models models as special cases described in
#'   (Meyer, et al., 2019) and (Wooten, et al., 2021).
#'
#'   The functional form is
#'   \preformatted{
#'     <response> ~ MuSyC(
#'       <treatment 1> - <logd1scale>,
#'       <treatment 2> - <logd2scale>,
#'       logE0,
#'       logC1, logE1, h1,
#'       logC2, logE2, h2,
#'       logE3, logalpha)}
#'  See [MuSyC()] for the full mathematical description of the
#'  `MuSyC` function. By default the observed data (and therefore should be
#'  columns in the input data data.frame) are
#'
#'    * **`<treatment 1>`**: `logd1`, the `log10` of the dose
#'      as a molar concentration of treatment 1
#'    * **`<treatment 2>`**: `logd2`, the `log10` of the dose
#'      as a molar concentration of treatment 2
#'    * **`<response>`**: `response`, with unspecified units
#'
#'  The `logd1scale` and `logd2scale` are used to center `<treatment 1>`
#'  and `<treatment 2>` to make fitting more numerically stable. If they are not
#'  in the input `data`, then they are taken to be the mean of
#'  `<treatment 1>` and `<treatment 2>` respectively.
#'
#'  The modeled parameters are
#'  \itemize{
#'    \item{**`logE0`**: the `log(<response>)` when `<treatment 1> = 0` and
#'      `<treatment 2> = 0`}
#'    \item{**`logC1`**: the `log(<treatment 1>)` where when `<treatment 2> =
#'      0`, the `<response>` is halfway between `E0` and `E1`}
#'    \item{**`logE1`**: the `log(response)` when `<treatment 1> => Inf` and
#'      `<treatment 2> = 0`}
#'    \item{**`h1`**: the hill slope of the response with respect to
#'      `<treatment 1>` when `<treatment 1> = C1` and `<treatment 2> = 0`. See
#'      [MuSyC_hi_to_si()] and [MuSyC_si_to_hi()] for
#'      converting between the slope (si) and hill slope (hi).}
#'    \item{**`logC2`**: the `log(<treatment 2>)` where when `<treatment 1> =
#'      0`, the `<response>` is halfway between `E0` and `E2`}
#'    \item{**`logE2`**: the `log(response)` when `<treatment 2> => Inf` and
#'      `<treatment 1> = 0`}
#'    \item{**`h2`**: the hill slope of the response with respect to
#'      `<treatment 2>` when `<treatment 2> = C2` and `<treatment 1> = 0`. See
#'      [MuSyC_hi_to_si()] and [MuSyC_si_to_hi()] for converting between the
#'      slopec(si) and hill slope (hi).}
#'    \item{**`logE3`**: the `log(response)` when `<treatment1 1> => Inf` and
#'      `<treatment 2> => Inf`, modeling the synergistic *efficacy*}
#'    \item{**`logalpha`**: the `log` of the synergistic *potency* `alpha`.
#'      When `alpha > 1` the treatments are synergistic so that `<treatment
#'      1>`` shifts the response due to `<treatment 2>` to lower doses and visa
#'      versa. When `alpha < 1` the treatments are antagonistic so that
#'      `<treatment 1>` shifts the response to `<treatment 2>` to higher doses
#'      and vise versa}}
#'
#' @param data `data.frame` of observed data. It must contain columns
#'   for the treatment, response and any additional predictors specified in the
#'   formula. See [sigmoid_agonist_formula()] for more details.
#' @param formula `bpformula` object. To create a formula for the MuSyC
#'   model, use the [MuSyC_formula()] function.
#' @param prior [brms::brmsprior()] for the model parameters. To
#'   create a prior for the MuSyC model, use the [MuSyC_prior()]
#'   function.
#' @param init `function` giving the initial values for the parameters. To
#'   specify the initial values, use the [MuSyC_init()] function.
#' @param control a named `list` of arguments to control the sampler's
#'   behavior. Adding `max_treedepth` and giving a greater value than
#'   `10` can improve model convergence.
#' @param stanvars `stanvars` code for the MuSyC model.
#' @param expose_functions `logical`. Expose the sigmoid function used in
#'   the model. This is needed e.g. for [brms::loo_compare()]
#' @param ... additional arguments passed to [brms::brm()]
#'
#' @references
#'   Meyer, D.J., Wooten, D.J., Paudel B.B., Bauer, J., Hardeman, K.N.,
#'   Westover, D., Lovly, C.M., Harris, L.A., Tyson D.R., Quaranta, V.,
#'   Quantifying Drug Combination Synergy along Potency and Efficacy Axes, Cell
#'   Syst. 8, 2 (2019). https://doi.org/10.1016/j.cels.2019.01.003
#'
#'   Wooten, D.J., Meyer, C.T., Lubbock, A.L.R. et al. MuSyC is a consensus
#'   framework that unifies multi-drug synergy metrics for combinatorial drug
#'   discovery. Nat Commun 12, 4607 (2021).
#'   https://doi.org/10.1038/s41467-021-24789-z
#'
#' @importFrom rlang .data
#' @export
MuSyC_model <- function(
  data,
  prior = MuSyC_prior(),
  init = MuSyC_init(),
  formula = MuSyC_formula(),
  control = list(
    adapt_delta = .99,
    max_treedepth = 12),
  stanvars = c(
    MuSyC_stanvar(),
    MuSyC_genquant()),
  expose_functions = TRUE,
  ...) {

  args <- list(...)

  if (!inherits(formula, "bpformula")) {
    warning(
      "formula must be a 'bpformula'. You can use the ",
      "'BayesPharma::MySYC_formula(...)' formula function.")
  }

  if (!(formula$bayes_pharma_info[["treatment_1_variable"]] %in% names(data))) {
    warning(
      paste0(
        "There needs to be variable for treatment 1 '",
        formula$bayes_pharma_info[["treatment_1_variable"]], "' ",
        "as a column in the input 'data' data.frame\n"))
  }

  if (!(formula$bayes_pharma_info[["treatment_2_variable"]] %in% names(data))) {
    warning(
      paste0(
        "There needs to be variable for treatment 2 '",
        formula$bayes_pharma_info[["treatment_2_variable"]], "' ",
        "as a column in the input 'data' data.frame\n"))
  }

  if (!inherits(prior, "brmsprior")) {
    warning(
      "prior must be a 'brmsprior'. You can use the ",
      "'BayesPharma::MuSyC_prior(...)' function.")
  }

  # To make the model more stable, the log dose values should be small.
  # So if not provided, add a scale the dose by the mean of the input.
  # This strategy allows keeping the parameter estimates more interpretable
  if (!("logd1scale" %in% names(data))) {
    data <- data |>
      dplyr::mutate(logd1scale = mean(
        .data[[
          formula$bayes_pharma_info[["treatment_1_variable"]]
        ]]))
  }

  if (!("logd2scale" %in% names(data))) {
    data <- data |>
      dplyr::mutate(logd2scale = mean(
        .data[[
          formula$bayes_pharma_info[["treatment_2_variable"]]
        ]]))
  }

  init <- eval_init(
    init,
    sdata = brms::make_standata(
      formula = formula,
      data = data,
      prior = prior,
      ...),
    algorithm = args$algorithm,
    chains = args$chains)

  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    control = control,
    stanvars = stanvars,
    ...)

  model$bayes_pharma_info <- list(model_type = "MuSyC")

  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    list(formula_info = formula$bayes_pharma_info))

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  class(model) <- c("bpfit", class(model))
  model
}
