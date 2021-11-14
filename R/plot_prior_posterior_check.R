#'Posterior Predictive Check
#'
#'pp_check from the brms package has different plot types to analyze the model
#'fit by comparing the observed data with generated data from the model.
#'
#'Here is the documentation for pp_check:
#'http://paul-buerkner.github.io/brms/reference/pp_check.brmsfit.html
#'
#'Here is the documentation for pp_check plot types under PPC plotting functions':
#'https://mc-stan.org/bayesplot/reference/PPC-overview.html
#'
#'The plot function can be entered as a string for argument 'plot_type' in the
#'function
#'
#'@param model brmsfit.
#'@param plot_type string. Name of available plot types.
#'@param n numeric value. Number of posterior draws to be used.
#'
#'@return ggplot object.
#'
#'@export

plot_pp_check <- function(model,
                          plot_type = "dens_overlay",
                          n = 50,
                          ...) {
  cat("For other plot options reference 'PPC plotting functions' on this
  website https://mc-stan.org/bayesplot/reference/PPC-overview.html")

  brms::pp_check(model, type = plot_type, ndraws = n, ...)
}
