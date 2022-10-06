





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
    inits = MuSyC_inits(),
    formula = MuSyC_formula(),
    control = list(
      adapt_delta = .99,
      max_treedepth = 12),
    ...) {
  
  brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = c(
      MuSyC_function_stanvar,
      MuSyC_genquant_stanvar),
    ...)
}
