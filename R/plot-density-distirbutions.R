#' Create a plot of the density distributions of modeled parameters from brmsfit
#' model
#'
#' @description This function is useful to observe the distributions
#'     of the priors set for the brmsfit model. By adding
#'     \code{sample_prior = "only"} to the sigmoid_model or a brmsfit
#'     model, the model samples only from the prior distributions and
#'     can be observed using this plot function to verify that the
#'     values intended to be covered are being included.
#'
#' @param model brmsfit model.
#' @param pars parameters to choose (Default b_<parameter>_Intercept')
#' @param labeller strip off the 'b_' and '_Intercept' from the parameter labels
#' @param title_label string of the plot title. (Default = "Prior Density Plot")
#'
#' @return ggplot2::ggplot object.
#'
#' @examples
#'\dontrun{
#'   density_distributions_plot(
#'     model = my_sigmoid_model)
#'}
#'
#' @importFrom rlang .data
#' @export
density_distributions_plot <- function(
  model,
  pars = NULL,
  labeller = NULL,
  title_label = "Density Distributions") {

  if (is.null(pars)) {
    pars <- dplyr::vars(tidyselect::starts_with("b_"))
  }

  if (is.null(labeller)) {
    labeller <- function(x) {
      x |>
        dplyr::mutate(
          Parameter = .data["Parameter"] |>
            stringr::str_replace_all("b_|_Intercept", ""))
    }
  }

  # generate plot
  plot <- bayesplot::mcmc_dens(
    x = model,
    pars = pars)

  # Add style
  plot +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Parameter Value",
      y = "Density") +
    ggplot2::facet_wrap(
      ~ Parameter,
      scales = "free",
      labeller = labeller)
}
