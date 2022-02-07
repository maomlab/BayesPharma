#' Create a plot of the density distributions of modeled parameters from brmsfit model
#'
#' @description This function is useful to observe the distributions of the priors set for
#'   the brmsfit model. By adding `sample_prior = "only"` to the dr_model or a brmsfit model,
#'   the model samples only from the prior distributions and can be observed using this plot function
#'   to verify that the values intended to be covered are being included.
#'
#' @usage
#'   density_distributions(model = <model>,
#'                         predictors_col_name = <predictors_col_name>,
#'                         half_max_label = <ec50>,
#'                         title_label = <title_label>,
#'                         sample_type = <sample_type>)
#'
#' @param model brmsfit model.
#' @param predictors_col_name string expression for predictors column in the input data.frame (default = "_Intercept").
#'   Predictors are the perturbations tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the type of
#'   experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param title_label string of the plot title. (Default = "Prior Density Plot")
#' @param sample_type string of the type of density distribution (i.e. Prior or Posterior). (default = "Prior")
#' @return ggplot2::ggplot object.
#'
#' @examples
#'   density_distributions(model = my_dr_model_priors,
#'                         predictors_col_name = "predictors",
#'                         half_max_label = "ic50",
#'                         title_label = "Parameter Density Distribution Plots",
#'                         sample_type = "Prior")
#'
#' @export

density_distributions <- function(model,
                                  predictors_col_name = "_Intercept",
                                  half_max_label = NULL,
                                  title_label = "Prior Density Distribution Plots",
                                  sample_type = "Prior") {
  prior <- dplyr::bind_rows(
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::mutate(sample_type = sample_type) %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma"))
  ) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "b_[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("b_")) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove(predictors_col_name)) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_replace("ec50", half_max_label))

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

#' Displays a data.frame of basic statistical information about the model results
#'
#' @description data.frame containing summary statistics of brmsfit model.
#'   The summary statistics included are mean, median, standard deviation,
#'   lower confidence interval, and upper confidence interval.
#'
#' @usage
#'   basic_stats(model = <model>,
#'               predictors_col_name = <predictors_col_name>,
#'               half_max_label = <ec50>,
#'               l_ci = <l_ci>,
#'               u_ci = <u_ci>)
#'
#' @param model brmsfit model
#' @param predictors_col_name string expression for predictors column in the input data.frame (default = "predictors").
#'   Predictors are the perturbations tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the type of
#'   experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param l_ci numeric unit of the lower confidence interval (default = 0.025)
#' @param u_ci numeric unit of the upper confidence interval (default = 0.975)
#' @return tibble::tibble object.
#'
#' @examples
#'   basic_stats(model = my_dr_model,
#'               predictors_col_name = "predictors",
#'               half_max_label = "ic50",
#'               l_ci = 0.025,
#'               u_ci = 0.975)
#'
#' @export

basic_stats <- function(model,
                        predictors_col_name = "predictors",
                        half_max_label = NULL,
                        l_ci = 0.025,
                        u_ci = 0.975) {

  ple_info <- brms::fixef(model, probs = c(l_ci, u_ci))
  print(paste0("lower CI:", l_ci," upper CI:", u_ci))

  model %>%
    posterior::summarise_draws("mean",
                               "sd",
                               "median") %>%
    dplyr::filter(!stringr::str_detect(variable, "__$")) %>%
    dplyr::filter(!stringr::str_detect(variable, "sigma")) %>%
    dplyr::select(-variable) %>%
    cbind(l_ci = c(ple_info[, 3]),
          u_ci = c(ple_info[, 4])) %>%
    tibble::rownames_to_column("variables") %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove(predictors_col_name)) %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_replace("ec50", half_max_label))

}


#' Create a plot of the posterior density distributions of modeled parameters from brmsfit model
#'
#' @description plots the brmsfit model density distribution of each parameter
#'   and the mean, lower confidence interval, and upper confidence interval.
#'
#' @usage
#'   posterior_densities(model = <model>,
#'                       predictors_col_name = <predictors_col_name>,
#'                       half_max_label = <half_max_label>,
#'                       l_ci = <l_ci>,
#'                       u_ci = <u_ci>,
#'                       title_label = <title_label>)
#'
#' @param model brmsfit model.
#' @param predictors_col_name string expression for predictors column in the input data.frame (default = "predictors).
#'   Predictors are the perturbations tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the type of
#'   experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param l_ci numeric unit of the lower confidence interval (default = 0.025)
#' @param u_ci numeric unit of the upper confidence interval (default = 0.975)
#' @param title_label string of the plot title. (default = "Posterior Density Plots with Mean and 95% CI")
#' @return ggplot2::ggplot object.
#'
#' @examples
#'   posterior_densities(model = my_dr_model,
#'                       predictors_col_name = "predictors",
#'                       half_max_label = "ic50",
#'                       l_ci = 0.025,
#'                       u_ci = 0.975,
#'                       title_label = "Posterior Density Plots with Mean and 95% CI")
#'
#'@export

posterior_densities <- function(model,
                                predictors_col_name = "_Intercept",
                                half_max_label = NULL,
                                l_ci = 0.025,
                                u_ci = 0.975,
                                title_label = "Posterior Density Plots with Mean and 95% CI") {
  posterior <- dplyr::bind_rows(
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma"))
  ) %>%
    dplyr::rename(variables = .variable) %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "b_[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("b_")) %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove(predictors_col_name)) %>%
    dplyr::mutate(variables = stringr::str_extract(variables,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_replace("ec50", half_max_label))

  ggplot2::ggplot(data = posterior) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = .value),
      fill = "cyan2",
      color = "black",
      alpha = .9) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = mean),
      basic_stats(model, predictors_col_name, half_max_label, l_ci = l_ci, u_ci = u_ci),
      color = "red"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -Inf, xmax = l_ci, ymin = -Inf, ymax = Inf),
      basic_stats(model, predictors_col_name, half_max_label),
      color = "gray",
      alpha = 0.5) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = u_ci, xmax = Inf, ymin = -Inf, ymax = Inf),
      basic_stats(model, predictors_col_name, half_max_label),
      color = "gray",
      alpha = 0.5) +
    ggplot2::ggtitle(
      label = paste0(title_label)) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(variables),
      scales = "free") +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::scale_x_continuous("Parameter Value") +
    ggplot2::scale_fill_discrete("Distribution")
}

#' Create a plot of the prior & posterior density distributions of modeled parameters from brmsfit model
#'
#' @description Plot of the prior and posterior density distributions of each parameter from brmsfit model.
#'   The prior and posterior density distributions will be displayed on the same plot and color labeled.
#'
#' @usage
#'   prior_posterior_densities(model = <model>,
#'                             predictors_col_name = <predictors_col_name>,
#'                             half_max_response = <half_max_response>,
#'                             title_label = <title_label>)
#'
#' @param model brmsfit model.
#' @param predictors_col_name string expression for predictors column in the input data.frame (default = "_Intercept").
#'   Predictors are the perturbations tested during the experiment (i.e. Drug, Temperature, etc.).
#' @param half_max_label string of the label for the half maximal that fits the type of
#'   experiment that was done (i.e. ec50, ic50, ed50, id50, ld50, etc.).
#' @param title_label string of the plot title. (default = "Prior Posterior Density Plots")
#' @return ggplot2::ggplot object.
#'
#' @examples
#'   prior_posterior_densities(model = my_dr_model,
#'                             predictors_col_name = "predictors",
#'                             half_max_response = "ic50",
#'                             title_label = "Prior Posterior Density Plots")
#'
#' @export

prior_posterior_densities <- function(model,
                                      predictors_col_name = "_Intercept",
                                      half_max_label = NULL,
                                      title_label = "Prior Posterior Density Plots") {

  model_prior <- model %>%
    brms:::update.brmsfit(sample_prior = "only")

  draws <- dplyr::bind_rows(

    model_prior %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::mutate(sample_type = "Prior") %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma")),
    model %>%
      tidybayes::tidy_draws() %>%
      tidybayes::gather_variables() %>%
      dplyr::mutate(sample_type = "Posterior") %>%
      dplyr::filter(!stringr::str_detect(.variable, "__$")) %>%
      dplyr::filter(!stringr::str_detect(.variable, "sigma"))
  ) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "b_[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove("b_")) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_remove(predictors_col_name)) %>%
    dplyr::mutate(.variable = stringr::str_extract(.variable,
                                                   "[a-zA-Z0-9]+.{1,100}") %>%
                    stringr::str_replace("ec50", half_max_label))

  ggplot2::ggplot(data = draws) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_density(
      ggplot2::aes(x = .value,
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
