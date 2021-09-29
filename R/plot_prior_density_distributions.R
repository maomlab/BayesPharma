#'Create a plot of the prior density distributions of modeled parameters
#'
#'@param model brmsfit of sample_prior = "only"
#'@param title_label string
#'@return ggplot2::ggplot object
#'
#'@export

prior_densities <- function(model,
                            title_label = "Prior Density Plots") {
  prior <- dplyr::bind_rows(
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::mutate(sample_type = "Prior") %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma"))
  ) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "b_[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("b_")) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("predictors"))

  ggplot2::ggplot(data = prior) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = .value),
      fill = "hotpink2",
      color = "black",
      alpha = .9) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.variable),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_discrete("Distribution")
}
