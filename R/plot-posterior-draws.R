#' Plot of Posterior Model Fit Draws
#'
#' @description A plot of a sample of model fit draws from the
#'     posterior distribution from the expected mean and median
#'     quantile intervals.
#'
#' @param model brmsfit model.
#' @param newdata data.frame of newdata to use for
#'     predictions. Default data.frame with each predictor and
#'     log-dose.
#' @param n numeric value of the number of draws to be observed
#'     (default = 50).
#' @param point_size numeric. \code{geom_jitter} point size (default =
#'     0.75).
#' @param jitter_height numeric. the height distance between
#'     overlapping points (default = 0).
#' @param jitter_width numeric. the width distance between overlapping
#'     points (default = 0).
#' @param title character name for the plot (default =
#'     "Dose-Response Posterior Draws").
#' @param xlab character name for the x-axis label (default:
#'     \code{treatment_units}).
#' @param ylab character name for the y-axis label (default =
#'     "Response").
#' @returns ggplot2::ggplot object.
#'
#' @examples
#'\dontrun{
#'   # Consider a model named my_model and data named my_data with a column
#'   # named predictors containing multiple different perturbations.
#'   posterior_draws_plot(
#'     model = my_model,
#'     newdata = my_data,
#'     predictors_col_name = "predictors",
#'     lower = -12,
#'     upper = -3,
#'     n = 50,
#'     facet_var = predictors,
#'     jitter_width = 0.10,
#'     title = "Dose-Response Posterior Draws",
#'     xlab = "Log[Molar]",
#'     ylab = "Response")
#'}
#'
#' @importFrom rlang :=
#' @export
posterior_draws_plot <- function(
    model,
    newdata = NULL,
    n = 50,
    point_size = 0.75,
    jitter_height = 0,
    jitter_width = 0,
    title = "Dose-Response Posterior Draws",
    xlab = NULL,
    ylab = NULL) {

  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "posterior_draws_plot expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }

  info <- model$bayes_pharma_info$formula_info

  if (!((info$treatment_dimension == 1) && (info$response_dimension == 1))) {
    warning(paste0(
      "posterior_draws_plot expects the treatment and response ",
      "to be one dimension each. Instead, the treatment has dimension ",
      info$treatment_dimension, " and the response has dimension ",
      info$response_dimension, "."))
  }

  # expand out all combinations of the predictor
  # and add a sequence of values along the log_dose dimension
  if (is.null(newdata)) {
    predictor_values <- model$data |>
      dplyr::select(-tidyselect::any_of(c(
        info$treatment_variable, info$response_variable))) |>
      as.list() |>
      purrr::map(unique)
    treatment_range <- model$data[
      model$data[[info$tretment_variabl]] |> is.finite() |> which(),
      info$tretment_variable] |>
      range()
    treatment_values <- list(
      treatment = seq(
        from = treatment_range[1],
        to = treatment_range[2],
        length.out = 100))
    newdata <- do.call(
      what = tidyr::expand_grid,
      args = c(predictor_values, treatment_values))
  }

  predictor_names <- newdata |>
    names() |>
    purrr::keep(~. != "log_dose")

  if (length(predictor_names) > 0) {
    facets_layer <- list(
      ggplot2::facet_wrap(
        facets = paste0("~", paste(predictor_names, collapse = "+")) |>
          stats::as.formula()))
  } else {
    facets_layer <- NULL
  }

  # this makes the "hair"
  ep_data <- model |>
    tidybayes::add_epred_draws(
      newdata = newdata,
      value = "response",
      re_formula = NA,
      ndraws = n)

  # this makes the ribbon
  pp_data <- model |>
    tidybayes::add_predicted_draws(
      newdata = newdata,
      value = "response",
      re_formula = NA,
      ndraws = 200)  |>
    ggdist:: median_qi(.width = c(.5, .8, .95))

  if (is.null(xlab)) {
    if (!is.null(info$treatment_units)) {
      xlab <- info$treatment_units
    } else {
      xlab <- "Treatment"
    }

    if (!is.null(info$response_units)) {
      ylab <- info$response_units
    } else {
      ylab <- "Response"
    }
  }


  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_discrete(
      "Median Quantile Interval",
      labels = c("95%", "80%", "50%")) +
    ggdist::geom_lineribbon(
      data = pp_data,
      mapping = ggplot2::aes(
        x = .data[[info$treatment_variable]],
        y = .data[[info$response_variable]],
        ymin = .data[[".lower"]],
        ymax = .data[[".upper"]]),
      alpha = .15) +
    ggplot2::geom_line(
      data = ep_data,
      mapping = ggplot2::aes(
        x = .data[[info$treatment_variable]],
        y = .data[[info$response_variable]],
        group = .data[[".draw"]]),
      linewidth = 0.4,
      alpha = 0.2,
      color = "blueviolet") +
    ggplot2::geom_jitter(
      data = model$data,
      mapping = ggplot2::aes(
        x = .data[[info$treatment_variable]],
        y = .data[[info$response_variable]]),
      size = point_size,
      width = jitter_width,
      height = jitter_height) +
    facets_layer +
    ggplot2::labs(title = title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
}
