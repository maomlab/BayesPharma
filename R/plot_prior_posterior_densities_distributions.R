#'
#'
#'
#'
#'
#'

prior_posterior_densities <- function(model,
                                      title_label = "Prior Posterior Density Plots") {

  model_prior <- model %>%
    brms:::update.brmsfit(sample_prior = "only")

  draws <- dplyr::bind_rows(
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::mutate(sample_type = "Posterior") %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma")),
    model_prior %>%
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

  ggplot2::ggplot(data = draws) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      ggplot2::aes( x = .value,
                    group = sample_type,
                    fill = sample_type),
      color = "black",
      alpha = .7) +
  ggplot2::ggtitle(
    label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(.variable),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_manual(
      values = c("Posterior" = "cyan2", "Prior" = "hotpink2"))
}
