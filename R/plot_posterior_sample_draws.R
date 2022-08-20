
#' Plot of Posterior Model Fit Draws
#'
#' @description A plot of a sample of model fit draws from the posterior
#' distribution from the expected mean and median quantile intervals.
#'
#' @param model brmsfit model.
#' @param data data.frame of data inputted into the model.
#' @param predictors_col_name character. The name of the column containing the
#'   predictors for the model fit. If there is only one predictor, then any
#'   character input can be used and will not affect the plot (default = "na").
#' @param lower numeric value of the lower bound of log_dose to be observed
#'   (default = smallest data$log_dose value > -Inf).
#' @param upper numeric value of the upper bound of log_dose to be observed
#'   (default = greatest data$log_dose value < Inf).
#' @param n numeric value of the number of draws to be observed (default = 50).
#' @param facet_var defined variable to determine the facets of the plot. For
#'   models with multiple predictors, include the predictor column name;
#'   otherwise, include the character name of your choosing.
#' @param point_size numeric. geom_jitter point size (default = 0.75).
#' @param jitter_height numeric. the height distance between overlapping points
#'   (default = 0).
#' @param jitter_width numeric. the width distance between overlapping points
#'   (default = 0).
#' @param title character name for the plot
#'   (default = "Dose-Response Posterior Draws").
#' @param xlab character name for the x-axis label (default = "Log[Molar]").
#' @param ylab character name for the y-axis label (default = "Response").
#' @return ggplot2::ggplot object.
#'
#' @examples
#'\dontrun{
#'   # Consider a model named my_model and data named my_data with a column
#'   # named predictors containing multiple different perturbations.
#'   posterior_draws_plot(model = my_model, data = my_data,
#'                        predictors_col_name = "predictors",lower = -12,
#'                        upper = -3, n = 50, facet_var = predictors,
#'                        jitter_width = 0.10,
#'                        title = "Dose-Response Posterior Draws",
#'                        xlab = "Log[Molar]", ylab = "Response")
#'}
#' @export

posterior_draws_plot <- function(
    model,
    data = NULL,
    predictors_col_name = "na",
    lower = NULL,
    upper = NUL,
    n = 50,
    facet_var = NULL,
    point_size = 0.75,
    jitter_height = 0,
    jitter_width = 0,
    title = "Dose-Response Posterior Draws",
    xlab = "Log[Molar]",
    ylab = "Response") {

  if (is.character(predictors_col_name) == FALSE) {
    warning("predictors_col_name must be a character. If there are not a
            predictors in the data and model, then run using the default
            argument (predictors_col_name = 'na').")
  } else if (predictors_col_name == "na") {
    warning("predictors_col_name is set to default argument 'na'. If predictors
            are present in the data and model, assign the column name to
            predictors_col_name.")
  }
  
  if(is.null(data)){
    data <- model$data
  }
  
  lower <- data |>
    dplyr::filter(log_dose > -Inf) |>
    purrr::pluck("log_dose") |>
    min(na.rm = TRUE)
  upper <- data |>
    dplyr::filter(log_dose < Inf) |>
    purrr::pluck("log_dose") |>
    max(na.rm = TRUE)
  
  ep_data <- model %>%
    tidybayes::add_epred_draws(
      newdata = tidyr::expand_grid(
        log_dose = seq(from = lower, to = upper, length.out = n),
        {{predictors_col_name}} := data[[predictors_col_name]] %>% unique()),
      value = "response",
      re_formula = NA,
      ndraws = n)

  pp_data <- model %>%
    tidybayes::add_predicted_draws(
      newdata = tidyr::expand_grid(
        log_dose = seq(from = lower, to = upper, length.out = n),
        {{predictors_col_name}} := data[[predictors_col_name]] %>% unique()),
      value = "response",
      re_formula = NA)  %>%
    ggdist:: median_qi(.width = c(.5, .8, .95))

  plot <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_discrete(
      "Median Quantile Interval",
      labels = c("95%", "80%", "50%")) +
    ggdist::geom_lineribbon(
      data = pp_data,
      mapping = ggplot2::aes(
        x = log_dose,
        y = response,
        ymin = .lower,
        ymax = .upper),
      alpha = .15) +
    ggplot2::geom_line(
      data = ep_data,
      mapping = ggplot2::aes(
        x = log_dose,
        y = response,
        group = .draw),
      size = 0.4,
      alpha = 0.2,
      color = "blueviolet") +
    ggplot2::geom_jitter(
      data = data,
      mapping = ggplot2::aes(
        x = log_dose,
        y = response),
      size = point_size,
      width = jitter_width,
      height = jitter_height) +
    ggplot2::labs(title = title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  if(!is.null(substitute(facet_var))){
    plot <- plot +
      ggplot2::facet_wrap(facets = dplyr::vars({{facet_var}}))
  }
  plot
}
