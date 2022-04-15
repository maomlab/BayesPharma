#' Perform Posterior Predictive Check
#'
#' @description Given a brms model, perform a graphical posterior predictive
#'   check.
#'
#' pp_check from the brms package has different plot types to analyze the model
#' fit by comparing the observed data with generated data from the model.
#'
#' Here is the documentation for pp_check:
#' http://paul-buerkner.github.io/brms/reference/pp_check.brmsfit.html
#'
#' Here is the documentation for pp_check plot types under PPC plotting
#' functions: https://mc-stan.org/bayesplot/reference/PPC-overview.html
#'
#' @param model brmsfit model.
#' @param plot_type String expression of a plot function.
#' @param n numeric units of posterior draws to be used.
#' @return ggplot object.
#'
#' @examples
#'\dontrun{
#' Consider a dose response model with the plot type being 10 box plots
#' plot_pp_check(model = my_dose_response_model,
#'               plot_type = "box_plot",
#'               n = 10)
#'}
#' @export

plot_pp_check <- function(model,
                          plot_type = "dens_overlay",
                          n = 50,
                          ...) {

  brms::expose_functions(model, vectorize = TRUE)

  brms::pp_check(model, type = plot_type, ndraws = n, ...)
}
