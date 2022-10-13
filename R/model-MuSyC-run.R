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
#' @param data data.frame of experimental data.
#'   must contain columns \code{response} and any predictors specified in
#'   the formula.
#' @param formula \code{brmsformula} object. To create a dose-response
#'   \code{brmsformula}, use the \code{MuSyC_formula} function.
#' @param prior \code{brmspriors} for \code{kcat} and \code{kM} parameters. Use
#'   \code{BayesPharma::MuSyC_prior(...)} to create the prior to use here.
#'   Default: \code{BayesPharma::MuSyC_prior()}.
#' @param init Initial values for the parameters, Use
#'   \code{BayesPharma::MuSyC_init(...)} to create the init to use here. Default
#'   \code{BayesPharma::MuSyC_init()}.
#' @param control a named list of parameters to control the sampler's behavior.
#'   Adding \code{max_treedepth} and giving a greater value than 10 can improve model
#'   convergence (default: \code{list(adapt_delta = 0.99)}).
#' @param stanvars \code{stanvars} code for the model. Default defines the
#'   MuSyC function and genquant code.
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
    ...) {

  if (!("response" %in% names(data))) {
    warning(
      "There needs to be a column 'response' in the input 'data' data.frame\n")
  }

  if (!("logd1" %in% names(data))) {
    warning(paste0(
      "There needs to be a column 'logd1' in the input 'data' data.frame. ",
      "This is the log dose of treatment 1.\n"))
  }

  if (!("logd2" %in% names(data))) {
    warning(paste0(
      "There needs to be a column 'logd2' in the input 'data' data.frame. ",
      "This is the log dose of treatment 2.\n"))
  }

  # To make the model more stable, the log dose values should be small.
  # So if not provided, add a scale the dose by the mean of the input.
  # This strategy allows keeping the parameter estimates more intepretable
  if (!("logd1scale" %in% names(data))) {
    data <- data |>
      dplyr::mutate(logd1scale = mean(.data[["logd1"]]))
  }

  if (!("logd2scale" %in% names(data))) {
    data <- data |>
      dplyr::mutate(logd2scale = mean(.data[["logd2"]]))
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
  model


}
