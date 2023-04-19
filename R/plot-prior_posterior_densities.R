#' Plot Prior and Posterior Marginal Distributions
#' 
#' Generate a plot that shows for each model variable the distribution of the
#' marginal posterior uncertainty overlaid on the distribution of the marginal
#' prior uncertainty.
#'
#' @param model \code{bpfit} object resulting from fitting a BayesPharma model
#' @param predictors_col_name string expression for predictors column
#'     in the input data.frame (default = "_Intercept"). Predictors
#'     are the perturbations tested during the experiment (i.e. Drug,
#'     Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that
#'     fits the type of experiment that was done (i.e. ec50, ic50,
#'     ed50, id50, ld50, etc.).
#' @param title_label string of the plot title.  (default =
#'     "Prior Posterior Density Plots")
#' @returns \code{\link[ggplot]{ggplot}} object.
#'
#' @examples
#'\dontrun{
#'   plot_prior_posterior_densities(
#'     model = my_sigmoid_model,
#'     predictors_col_name = "predictors",
#'     half_max_response = "ic50",
#'     title_label = "Prior Posterior Density Plots")
#'}
#' @export
plot_prior_posterior_densities <- function(
  model,
  
  predictors_col_name = "_Intercept",
  half_max_label = "ec50",
  title_label = "Prior Posterior Density
  Plots") {

  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_prior_posterior_densities expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }
  
  info <- model$bayes_pharma_info$formula_info
  
  
  model_prior <- model |>
    stats::update(
      sample_prior = "only",
      iter = 2000)

  draws <- dplyr::bind_rows(
    model_prior |>
      tidybayes::tidy_draws() |>
      tidybayes::gather_variables() |>
      dplyr::mutate(sample_type = "Prior") |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "__$")) |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "sigma")) |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "lprior")),
    model |>
      tidybayes::tidy_draws() |>
      tidybayes::gather_variables() |>
      dplyr::mutate(sample_type = "Posterior") |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "__$")) |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "sigma")) |>
      dplyr::filter(!stringr::str_detect(.data[[".variable"]], "lprior"))) |>
    dplyr::mutate(
      .variable = .data[[".variable"]] |>
        stringr::str_extract("b_[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_remove("b_") |>
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_remove(predictors_col_name) |>
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_replace("ec50", half_max_label))

  ggplot2::ggplot(data = draws) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
         x = .data[[".value"]],
         group = .data[["sample_type"]],
         fill = .data[["sample_type"]]),
      color = "black",
      alpha = .7) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.data[[".variable"]]),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_manual(
      values = c("Posterior" = "cyan2", "Prior" = "hotpink2"))
}
