#' MuSyC Synergy Model
#'
#' Combined:
#'      sample1 =
#'
#'
#'      nlp_logE0 = X_logE0 * b_logE0
#'                  _______   _______
#'                     = 1    = param
#'
#'      nlp_logE1 = X_logE1 * b_logE0
#'                  _______   ______
#'                  =sample1
#'
#'
#' @param data \code{data.frame} of experimental data.  must contain columns
#'     \code{response} and any predictors specified in the formula.
#' @param formula \code{brmsformula} object. To create a dose-response
#'     \code{brmsformula}, use the \code{MuSyC_formula} function.
#' @param prior \code{brmspriors} for \code{kcat} and \code{kM}
#'     parameters. Use \code{BayesPharma::MuSyC_prior(...)} to create
#'     the prior to use here.  Default:
#'     \code{BayesPharma::MuSyC_prior()}.
#' @param init Initial values for the parameters, Use
#'     \code{BayesPharma::MuSyC_init(...)} to create the init to use
#'     here. Default \code{BayesPharma::MuSyC_init()}.
#' @param control a named list of parameters to control the sampler's
#'     behavior.  Adding \code{max_treedepth} and giving a greater
#'     value than 10 can improve model convergence (default:
#'     \code{list(adapt_delta = 0.99)}).
#' @param stanvars \code{stanvars} code for the model. Default defines
#'     the MuSyC function and genquant code.
#' @param expose_functions boolean. Expose the BayesPharma functions for the
#'   model [default: TRUE].
#' @param ... additional arguments passed to \code{brms::brm}
#'
#'
#'@export
MuSyC_model <- function(
    data,
    prior = MuSyC_prior(),
    init = MuSyC_init(),
    formula = MuSyC_formula(),
    control = list(
      adapt_delta = .99,
      max_treedepth = 12),
    stanvars = c(
      MuSyC_function_stanvar,
      MuSyC_genquant_stanvar),
    expose_functions = TRUE,
    ...) {

  if (!methods::is(formula, "bpformula")) {
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

  if (!methods::is(prior, "brmsprior")) {
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
      dplyr::mutate(logd1scale = mean(
        .data[[
          formula$bayes_pharma_info[["treatment_2_variable"]]
          ]]))
  }

  model <- brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    control = control,
    stanvars = stanvars,
    ...)

  model$bayes_pharma <- list(model_type = "MuSyC")

  model$bayes_pharma_info <- c(
    model$bayes_pharma_info,
    formula_info = formula$bayes_pharam_info)

  if (expose_functions) {
    brms::expose_functions(model, vectorize = TRUE)
  }

  model

  class(model) <- c("bpfit", class(model))
}
