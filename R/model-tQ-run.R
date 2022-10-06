#' BRMS model for the tQ enzyme kinetics model
#' @export
tQ_model <- function(
    data,
    formula = tQ_formula(),
    prior = tQ_prior(),
    init = tQ_init(),
    iter = 8000,
    control = list(adapt_delta = 0.99),
    ...) {

  brms::brm(
    formula = formula,
    data = data,
    prior = prior,
    init = init,
    iter = iter,
    control = control,
    stanvars = tQ_stanvar,
    ...)
}
