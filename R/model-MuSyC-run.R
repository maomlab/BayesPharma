





#'
#'
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
#'@export
fit_MuSyC_model <- function(
    data,
    prior = MuSyC_prior(),
    init = MuSyC_init(),
    formula = MuSyC_formula(),
    control = list(
      adapt_delta = .99,
      max_treedepth = 12),
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
      dplyr::mutate(logd1scale = mean(logd1))
  }
  
  if (!("logd2scale" %in% names(data))) {
    data <- data |>
      dplyr::mutate(logd2scale = mean(logd2))
  }
  
  brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    control = control,
    stanvars = c(
      MuSyC_function_stanvar,
      MuSyC_genquant_stanvar),
    ...)
}
