#' Plot of Posterior Model Fit Draws
#'
#' @description A plot of a sample of model fit draws from the
#'     posterior distribution from the expected mean and median
#'     quantile intervals.
#'
#' @param model `bpfit` object resulting from fitting a [BayesPharma] model
#' @param treatment_variable `character` or `NULL`. If `NULL` the treatment
#'   variable name will be looked up in the model. The treatment variable the
#'   `model$data` or `newdata` if supplied must have a column corresponding to
#'   the treatment variable.
#' @param treatment_units `character` or `NULL`. If `NULL` the treatment units
#'   will be looked up in the model. The treatment units will be used to label
#'   the X-axis of the plot.
#' @param treatment_from `numeric` or `NULL`, for the lower bound on the
#'   treatment range. If `NULL`, then use the (finite) lower bound of the
#'   treatment variable in the model data.
#' @param treatment_to `numeric` or `NULL`, for the upper bound on the
#'   treatment range. If `NULL`, then use the (finite) upper bound of the
#'   treatment variable in the model data.
#' @param response_variable `string` or `NULL`. If `NULL` the response variable
#'   name will be looked up in the model. The response variable the `model$data`
#'   or `newdata` if supplied must have a column corresponding to the response
#'   variable.
#' @param response_units `character` or `NULL`. If `NULL` the response units
#'   will be looked up in the model. The response units will be used to label
#'   the Y-axis of the plot.
#' @param newdata `data.frame` or `NULL` of new data to use for
#'   predictions. Default data.frame with each predictor and treatment variable.
#' @param n `numeric` value of the number of draws to show.
#' @param point_size `numeric`. [ggplot2::geom_jitter()] point size.
#' @param jitter_height `numeric`. the height distance between overlapping
#'   points.
#' @param jitter_width `numeric`. the width distance between overlapping points.
#' @param title character name for the plot
#' @param verbose `logical` give verbose output
#'
#' @returns [ggplot2::ggplot] object.
#'
#' @examples
#'\dontrun{
#'   # Consider a model named my_model and data named my_data with a column
#'   # named predictors containing multiple different perturbations.
#'   plot_posterior_draws(
#'     model = my_model,
#'     newdata = my_data,
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
#' @importFrom rlang .data
#' @export
plot_posterior_draws <- function(
  model,
  newdata = NULL,
  treatment_variable = NULL,
  treatment_units = NULL,
  treatment_from = NULL,
  treatment_to = NULL,
  response_variable = NULL,
  response_units = NULL,
  title = "Dose-Response Posterior Draws",
  n = 50,
  point_size = 0.75,
  jitter_height = 0,
  jitter_width = 0,
  verbose = FALSE) {

  if (!inherits(model, "bpfit")) {
    warning(paste0(
      "plot_posterior_draws expects model to be of class 'bpfit',",
      " instead it is of class ", class(model)))
  }

  if (is.null(treatment_variable)) {
    treatment_variable <- model |> get_treatment_variable()
  }

  if (is.null(treatment_units)) {
    treatment_units <- model |> get_treatment_units()
  }

  if (is.null(response_variable)) {
    response_variable <- model |> get_response_variable()
  }

  if (is.null(response_units)) {
    response_units <- model |> get_response_units()
  }

  # expand out all combinations of the predictor
  # and add a sequence of values along the treatment dimension
  if (is.null(newdata)) {
    predictor_values <- model$data |>
      dplyr::select(-tidyselect::any_of(c(
        treatment_variable, response_variable))) |>
      as.list() |>
      purrr::map(unique)
    if (verbose) {
      cat(
        "N predictor values: ",
        predictor_values |> purrr::map(length) |> paste0(collapse = " x "),
        "\n", sep = "")
    }

    # if the treatment range isn't given, then use the (finite) range of the
    # model data
    if (is.null(treatment_from) || is.null(treatment_to)) {
      treatment_range <- model$data[
        model$data[[treatment_variable]] |> is.finite() |> which(),
        treatment_variable] |>
        range()
      if (is.null(treatment_from)) {
        treatment_from <- treatment_range[1]
      }
      if (is.null(treatment_to)) {
        treatment_to <- treatment_range[2]
      }
    }
    treatment_values <- list(
      seq(
        from = treatment_from,
        to = treatment_to,
        length.out = 100))

    names(treatment_values) <- treatment_variable
    newdata <- do.call(
      what = tidyr::expand_grid,
      args = c(predictor_values, treatment_values))
  }

  if (verbose) {
    cat("Predicting data at ", nrow(newdata), " data points\n", sep = "")
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
  
  if (verbose) {
    cat(
      "Generating ", n, " hair for different epred draws from the posterior\n",
      sep = "")
  }
  
  # this makes the "hair"
  ep_data <- model |>
    tidybayes::add_epred_draws(
      newdata = newdata,
      value = response_variable,
      re_formula = NA,
      ndraws = n)

  # this makes the ribbon
  if (verbose) {
    cat(
      "Generating ribbon predition quantiles from 200 draws ",
      "from the posterior\n",
      sep = "")
  }
  
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
