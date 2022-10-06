#' Create a plot of the density distributions of modeled parameters from brmsfit
#' model
#'
#' @description This function is useful to observe the distributions of the
#'   priors set for the brmsfit model. By adding `sample_prior = "only"` to the
#'   dr_model or a brmsfit model, the model samples only from the prior
#'   distributions and can be observed using this plot function to verify that
#'   the values intended to be covered are being included.
#'
#' @param model brmsfit model.
#' @param predictors_col_name string expression for predictors column in the
#'   input data.frame (default = "_Intercept"). Predictors are the perturbations
#'   tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the
#'   type of experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param title_label string of the plot title. (Default = "Prior Density Plot")
#' @param sample_type string of the type of density distribution
#'   (i.e. Prior or Posterior). (default = "Prior")
#' @return ggplot2::ggplot object.
#'
#' @examples
#'\dontrun{
#'   density_distributions(
#'     model = my_dr_model_priors,
#'     predictors_col_name = "predictors",
#'     half_max_label = "ic50",
#'     title_label = "Parameter Density Distribution Plots",
#'     sample_type = "Prior")
#'}
#' @export

density_distributions <- function(
    model,
    predictors_col_name = "_Intercept",
    half_max_label = "ec50",
    title_label = "Density Distributions",
    sample_type = "Prior") {

  prior <- dplyr::bind_rows(
    model |>
      tidybayes::tidy_draws() |>
      tidybayes::gather_variables() |>
      dplyr::mutate(sample_type = sample_type) |>
      dplyr::filter(!stringr::str_detect(.variable, "__$")) |>
      dplyr::filter(!stringr::str_detect(.variable, "sigma")) |>
      dplyr::filter(!stringr::str_detect(.variable, "lprior"))) |>
    dplyr::mutate(
      .variable = .variable |>
        stringr::str_extract("b_[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_remove("b_") |>
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_remove(predictors_col_name) |>
        stringr::str_extract("[a-zA-Z0-9]+.{1,100}") |>
        stringr::str_replace("ec50", half_max_label))

  ggplot2::ggplot(data = prior) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = .value,
        group = sample_type,
        fill = sample_type),
      color = "black",
      alpha = .9) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.variable),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_manual(
      "Sample Type",
      values = c(
        "Posterior" = "cyan2",
        "posterior" = "cyan2",
        "Prior" = "hotpink2",
        "prior" = "hotpink2"),
      limits = c(sample_type))
}
