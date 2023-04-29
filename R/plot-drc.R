#' Create Plot of a DRC Model
#'
#' @param model `drc::drm` model object generated with [drc::drm()]
#' @param newdata `data.frame` where the predictions should be made.
#'   Default: use the range of treatment values in the model object.
#' @param predict_args `list` of arguments passed to
#'   `predict(model, ...)` including interval will generate ribbon of the
#'   given type, see [drc::predict.drc()] for more details.
#' @param aes_mapping `[ggplot2::aes](...)` mapping columns of the
#'   model data and new data to the plot. (Default:
#'   `ggplot2::aes](x = log_dose, y = response)`)
#' @param title `character` giving the plot title
#'
#'
#' @importFrom rlang .data
#' @export
plot_drc <- function(
    model,
    newdata = NULL,
    predict_args = list(interval = "prediction"),
    aes_mapping = ggplot2::aes(
      x = .data[["log_dose"]],
      y = .data[["response"]]),
    title =  NULL) {

  if (is.null(newdata)) {
    log_dose_range <- model$data$log_dose[
      model$data$log_dose |> is.finite() |> which()] |>
      range()
    newdata <- data.frame(
      log_dose = seq(
        from = log_dose_range[1],
        to = log_dose_range[2],
        length.out = 100))
  } else if (inherits(newdata, "data.frame")) {
    assertthat::assert_that("log_dose" %in% names(newdata))
  } else {
    warning(
      paste0(
        "newdata should either be NULL or a data.frame, instead it is of ",
        "class ", class(newdata), "\n"))
  }

  assertthat::assert_that(inherits(aes_mapping, "uneval"))
  assertthat::assert_that(
    is.null(title) || inherits(title, "character"))

  predicted_values <- newdata |>
    dplyr::bind_cols(
      do.call(
        what = stats::predict,
        args = c(list(
          object = model,
          newdata = newdata),
          predict_args))) |>
    dplyr::rename(
      response = .data[["Prediction"]],
      response_lower = .data[["Lower"]],
      response_upper = .data[["Upper"]])

  ggplot2::ggplot(mapping = aes_mapping) +
    ggplot2::theme_bw() +
    ggdist::geom_lineribbon(
      data = predicted_values,
      mapping = ggplot2::aes(
        ymin = .data[["response_lower"]],
        ymax = .data[["response_upper"]]),
      fill = "blue",
      alpha = .15) +
    ggplot2::geom_point(
      data = model$data) +
    ggplot2::labs(
      title = title) +
    ggplot2::scale_x_continuous("log[Concentration]") +
    ggplot2::scale_y_continuous("Response")
}
