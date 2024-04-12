#' Plot Density Distribution of Model Parameters
#'
#' @description This function is useful to observe the distributions
#'   of the priors set for the [brms::brmsfit] model. By adding
#'   `sample_prior = "only"` to the sigmoid_model or a [brms::brmsfit] model,
#'   the model samples only from the prior distributions and can be observed
#'   using this plot function to verify that the values intended to be covered
#'   are being included.
#'
#' @param model `[brms::brmsfit] model.
#' @param pars `character` vector of parameters or tidy parameter selection
#'.  using e.g. \pkg{tidyselect} to plot (Default: `'b_<parameter>_Intercept'`)
#' @param labeller `function` (Default: strip off the `'b_'` and `'_Intercept'`
#'   from the parameter labels).
#' @param title_label `character` of the plot title.
#'
#' @returns [ggplot2::ggplot()] object.
#'
#' @examples
#'\dontrun{
#'   plot_density_distribution(
#'     model = my_sigmoid_model)
#'}
#'
#' @importFrom rlang .data
#' @export
plot_density_distribution <- function(
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
          Parameter = .data[["Parameter"]] |>
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
