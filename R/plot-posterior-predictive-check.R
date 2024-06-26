#' Perform Posterior Predictive Check
#'
#' @description Given a \pkg{brms} model, perform a graphical posterior
#'     predictive check (PPC).
#'
#' [brms::pp_check()] has different plot types to analyze the model fit by
#' comparing the observed data with generated data from the model.
#'
#' Here is the documentation for [brms::pp_check]:
#' <http://paul-buerkner.github.io/brms/reference/pp_check.brmsfit.html>
#'
#' Here is the documentation for [bayesplot::pp_check] plot types under PPC
#' plotting functions:
#' <https://mc-stan.org/bayesplot/reference/PPC-overview.html>
#'
#' @param model [brms::brmsfit] model.
#' @param plot_type `character` for the plot type.
#' @param n `numeric` number of posterior draws to be used.
#' @param ... additional arguments to [brms::pp_check()].
#'
#' @returns [ggplot2::ggplot] object.
#'
#' @examples
#'\dontrun{
#' # Consider a dose response model with the plot type being 10 box plots
#' BayesPharma::plot_pp_check(
#'   model = my_dose_response_model,
#'   plot_type = "box_plot",
#'   n = 10)
#'}
#' @export
plot_pp_check <- function(
  model,
  plot_type = "dens_overlay",
  n = 50,
  ...) {

  brms::pp_check(model, type = plot_type, ndraws = n, ...)
}
