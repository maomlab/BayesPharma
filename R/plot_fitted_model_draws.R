
#' Dataframe of posterior samples of modeled parameters from brmsfit
#'
#'@param model brmsfit
#'@param n number of samples from the posterior distribution (default n = 100)
#'@return df that is required for the 'posterior_response_draws' function
#'

n_sample_draws <- function(model,
                           n= 100) {
  model %>%
    brms::posterior_samples() %>%
    dplyr::sample_n(n) %>%
    dplyr::mutate(draw_id = dplyr::row_number())
  }

#' Tibble of draw_id, drug, ec50, hill, top, bottom, and Response from a
#' sample of the posterior distribution
#'
#'@param model brmsfit
#'@param n number of samples from the posterior distribution (default n = 100).
#'@param lower number for the lowest log_dose value to be used for calculating
#'Response (default = -12).
#'@param upper number for the highest log_dose value to be used for calculating
#'Response (default = -3).
#'@param ec50 effective concentration that induces half of the maximum effect.
#'EC50 can be modeled by brmsfit or a fixed log dose value. (default = ec50)
#'@param hill slope factor. Hill can be modeled by brmsfit or a fixed value.
#'The standard slope for dose response curves is 1.0 for an agonist and -1.0
#'for an inhibitor. (default = hill)
#'@param top maximum response value. Top can be modeled by brmsfit or a fixed
#'value. (default = top)
#'@param bottom minimum response value. Bottom can be modeled by brmsfit or a
#'fixed value. (default = bottom)
#'@param drug_name string. The name of the drug being analyzed (default = drug)
#'@return tibble::tibble required for the 'plot_trajectories' function
#'
#'@export

posterior_response_draws <- function(model,
                                     n = 100,
                                     lower = -12,
                                     upper = -3,
                                     ec50 = ec50,
                                     hill = hill,
                                     top = top,
                                     bottom = bottom,
                                     drug_name = drug) {
  n_sample_draws(model, n) %>%
    tidyr::pivot_longer(cols = starts_with("b_"),
                        names_to = "Parameters",
                        values_to = "Value") %>%
    dplyr::mutate(b_class = stringr::str_extract(Parameters, "b_[a-zA-Z0-9]+") %>%
                    stringr::str_remove("b_"),
                  drug = stringr::str_extract(Parameters, "drug.+") %>%
                    stringr::str_remove("drug")) %>%
    dplyr::mutate() %>%
    dplyr::select(-Parameters) %>%
    tidyr::pivot_wider(id_cols = c("draw_id", "drug"),
                       names_from = "b_class",
                       values_from = "Value") %>%
    dplyr::mutate(drug = drug, ec50 = ec50, hill = hill, top = top, bottom = bottom) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      params <- .
      tibble::tibble(
        log_dose = c(
          seq(
            from = log10(1*10^(lower)),
            to = log10(1*10^(upper)),
            length.out = 100)),
        draw_id = params$draw_id,
        drug = params$drug,
        ec50 = params$ec50,
        hill = params$hill,
        top = params$top,
        bottom = params$bottom
      )
    }) %>%
    dplyr::mutate(Response = bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))) %>%
    dplyr::mutate(drug = ifelse(is.na(drug), replace_na(drug_name), drug))
  }

#' Tibble of drug, mean ec50, mean hill, mean top, mean bottom, and
#' mean Response of the posterior distribution.
#'
#'@param model brmsfit
#'@param n number of samples from the posterior distribution (default n = 100).
#'@param lower number for the lowest log_dose value to be used for calculating
#'Response (default = -12).
#'@param upper number for the highest log_dose value to be used for calculating
#'Response (default = -3).
#'@param ec50 effective concentration that induces half of the maximum effect.
#'EC50 can be modeled by brmsfit or a fixed log dose value. (default = ec50)
#'@param hill slope factor. Hill can be modeled by brmsfit or a fixed value.
#'The standard slope for dose response curves is 1.0 for an agonist and -1.0
#'for an inhibitor. (default = hill)
#'@param top maximum response value. Top can be modeled by brmsfit or a fixed
#'value. (default = top)
#'@param bottom minimum response value. Bottom can be modeled by brmsfit or a
#'fixed value. (default = bottom)
#'@param drug_name string. The name of the drug being analyzed (default = drug)
#'@return tibble::tibble required for the 'plot_trajectories' function
#'
#'@export

posterior_mean <- function(model,
                           n = 100,
                           lower = -12,
                           upper = -3,
                           ec50 = ec50,
                           hill = hill,
                           top = top,
                           bottom = bottom,
                           drug_name = drug) {
  model %>%
    brms::posterior_samples() %>%
    tidyr::gather(factor_key = TRUE) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(Mean = mean(value)) %>%
    dplyr::rename(variable = key) %>%
    dplyr::filter(!stringr::str_detect(variable, "__$")) %>%
    dplyr::filter(!stringr::str_detect(variable, "sigma")) %>%
    dplyr::mutate(b_class = stringr::str_extract(variable, "b_[a-zA-Z0-9]+")%>%
                    stringr::str_remove("b_"),
                  drug = stringr::str_extract(variable, "drug.+")%>%
                    stringr::str_remove("drug")) %>%
    dplyr::select(-variable) %>%
    tidyr::pivot_wider(id_cols = c("drug"),
                       names_from = "b_class",
                       values_from = "Mean") %>%
    dplyr::mutate(drug = drug, ec50 = ec50, hill = hill, top = top, bottom = bottom) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      params <- .
      tibble::tibble(
        log_dose = c(
          seq(
            from = log10(1*10^(lower)),
            to = log10(1*10^(upper)),
            length.out = 100)),
        drug = params$drug,
        ec50 = params$ec50,
        hill = params$hill,
        top = params$top,
        bottom = params$bottom
      )
    }) %>%
    dplyr::mutate(Response = bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))) %>%
    dplyr::mutate(drug = ifelse(is.na(drug), replace_na(drug_name), drug))
  }

#' Create a plot of the predicted responses from the posterior distribution
#'
#'@param data is the tibble or dataframe used for the brmsfit
#'@param measurement is the column in 'data' containing response values.
#'@param draws is the tibble::tibble returned from the
#''posterior_response_draws' function
#'@param pred_response is the 'Response' column in the tibble::tibble returned
#'from the 'posterior_response_draws' function.
#'@param mean_draws is the tibble::tibble returned from the 'posterior_mean'
#'function
#'@param title string for the plot title (default = NULL)
#'@param xlabel string for the x-axis label (default = NULL)
#'@param ylabel string for the y-axis label (default = NULL)
#'@return a ggplot2::ggplot object
#'
#'@export

plot_trajectories <- function(data,
                              drug_col_name = NULL,
                              measurement,
                              draws,
                              pred_response,
                              mean_draws,
                              title = NULL,
                              xlabel = "Log Dose",
                              ylabel = "Response") {


  input_data <- data %>%
    dplyr::rename(drug = drug_col_name)

  ggplot2::ggplot() +
    ggplot2::geom_point(data = input_data,
                        ggplot2::aes(x = log_dose,
                                     y = measurement),
                        size = 0.5,
                        color = "black") +
    ggplot2::geom_line(data = draws,
                       ggplot2::aes(x = log_dose,
                                    y = pred_response,
                                    group = draw_id),
                       size = 0.4,
                       alpha = 0.2,
                       color = "blueviolet") +
    ggplot2::geom_line(data = mean_draws,
                       ggplot2::aes(x = log_dose,
                                    y = Response),
                       size = 0.5,
                       alpha = 1.0,
                       color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title,
                  x = xlabel,
                  y = ylabel) +
    ggplot2::facet_wrap(
      facets = dplyr::vars(drug))
  }

#'
#'
#'
#'
#'
#'

plot_draws_data <- function(model,
                            n = 100,
                            lower = -12,
                            upper = -3,
                            ec50 = ec50,
                            hill = hill,
                            top = top,
                            bottom = bottom,
                            drug_name = drug,
                            data,
                            drug_col_name = NULL,
                            measurement,
                            draws,
                            pred_response,
                            mean_draws,
                            title = NULL,
                            xlabel = "Log Dose",
                            ylabel = "Response") {

  resp_draws <- posterior_response_draws(model,
                                         n,
                                         lower,
                                         upper,
                                         ec50,
                                         hill,
                                         top,
                                         bottom,
                                         drug_name)

  resp_draws_mean <- posterior_response_draws(model,
                                              n,
                                              lower,
                                              upper,
                                              ec50,
                                              hill,
                                              top,
                                              bottom,
                                              drug_name)

  draws_plot <- plot_trajectories(data,
                                  drug_col_name,
                                  measurement,
                                  draws = resp_draws,
                                  pred_response = resp_draws$Response,
                                  mean_draws = resp_draws_mean,
                                  title,
                                  xlabel,
                                  ylabel)
  return(draws_plot)


}
