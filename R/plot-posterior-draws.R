#' Plot of Posterior Model Fit Draws
#'
#' @description A plot of a sample of model fit draws from the
#'     posterior distribution from the expected mean and median
#'     quantile intervals.
#'
#' @param model bpfit object resulting from fitting a BayesPharma model
#' @param treatment_variable string or NULL. If NULL the treatment variable
#'     name will be looked up from the model formula. The treatment variable
#'     the model$data or newdata if supplied must have a column corresponding to
#'     the treatment variable (Default: NULL).
#' @param treatment_units string or NULL. If NULL the treatment units will
#'     be looked up in the model formula. The treatment units will be used to
#'     label the X-axis of the plot. (Default: NULL)
#' @param response_variable string or NULL. If NULL the response variable
#'     name will be looked up from the model formula. The response variable
#'     the model$data or newdata if supplied must have a column corresponding to
#'     the response variable (Default: NULL).
#' @param response_units string or NULL. If NULL the response units will
#'     be looked up in the model formula. The response units will be used to
#'     label the X-axis of the plot. (Default: NULL)
#' @param newdata data.frame of newdata to use for
#'     predictions. Default data.frame with each predictor and
#'     treatment variable.
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
    treatment_variable = NULL,
    treatment_units = NULL,
    response_variable = NULL,
    response_units = NULL,
    title = "Dose-Response Posterior Draws",
    n = 50,
    point_size = 0.75,
    jitter_height = 0,
    jitter_width = 0) {

  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "posterior_draws_plot expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }

  info <- model$bayes_pharma_info$formula_info

  if (is.null(treatment_variable)) {
    treatment_variable <- info$treatment_variable
    treatment_units <- info$treatment_units
    if (is.null(treatment_variable)) {
      stop(paste0(
        "Expected either treatment_variable and treatment_units to be ",
        "specified or both defined in the model$bayes_pharma_info"))
    }
  }

  if (is.null(response_variable)) {
    response_variable <- info$response_variable
    response_units <- info$response_units
    if (is.null(response_variable)) {
      stop(paste0(
        "Expected either response_variable and response_units to be ",
        "specified or defined in the model$bayes_pharma_info"))
    }
  }

  # expand out all combinations of the predictor
  # and add a sequence of values along the treatment dimension
  if (is.null(newdata)) {
    if (!(treatment_variable %in% names(model$data))) {
      stop(paste0(
        "Expected the treatment variable '", treatment_variable, "' to be a ",
        "column in the model$data, but instead it has columns ",
        "[", paste0(names(model$data), collapse = ", "), "]"))
    }

    if (!(response_variable %in% names(model$data))) {
      stop(paste0(
        "Expected the response variable '", response_variable, "' to be a ",
        "column in the model$data, but instead it has columns ",
        "[", paste0(names(model$data), collapse = ", "), "]"))
    }

    predictor_values <- model$data |>
      dplyr::select(-tidyselect::any_of(c(
        treatment_variable, response_variable))) |>
      as.list() |>
      purrr::map(unique)
    treatment_range <- model$data[
      model$data[[treatment_variable]] |> is.finite() |> which(),
      treatment_variable] |>
      range()
    treatment_values <- list(
      seq(
        from = treatment_range[1],
        to = treatment_range[2],
        length.out = 100))
    names(treatment_values) <- treatment_variable
    newdata <- do.call(
      what = tidyr::expand_grid,
      args = c(predictor_values, treatment_values))
  }

  predictor_names <- newdata |>
    names() |>
    purrr::keep(~. != treatment_variable)

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
      value = response_variable,
      re_formula = NA,
      ndraws = n)

  # this makes the ribbon
  pp_data <- model |>
    tidybayes::add_predicted_draws(
      newdata = newdata,
      value = response_variable,
      re_formula = NA,
      ndraws = 200)  |>
    ggdist:: median_qi(.width = c(.5, .8, .95))

  if (is.null(treatment_units)) {
    xlab <- treatment_variable
  } else {
    xlab <- paste(treatment_variable, treatment_units)
  }

  if (is.null(response_units)) {
    ylab <- response_variable
  } else {
    ylab <- paste(response_units, response_units)
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
        x = .data[[treatment_variable]],
        y = .data[[response_variable]],
        ymin = .data[[".lower"]],
        ymax = .data[[".upper"]]),
      alpha = .15) +
    ggplot2::geom_line(
      data = ep_data,
      mapping = ggplot2::aes(
        x = .data[[treatment_variable]],
        y = .data[[response_variable]],
        group = .data[[".draw"]]),
      linewidth = 0.4,
      alpha = 0.2,
      color = "blueviolet") +
    ggplot2::geom_jitter(
      data = model$data,
      mapping = ggplot2::aes(
        x = .data[[treatment_variable]],
        y = .data[[response_variable]]),
      size = point_size,
      width = jitter_width,
      height = jitter_height) +
    facets_layer +
    ggplot2::labs(title = title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
}
