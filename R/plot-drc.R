

#' Create plot of drc model
#' 
#' @param model drc object. drc model generated with drc::drm(...)
#' @param newdata data.frame where the predictions should be made. Default: use
#'   the range of log_dose values in the model object.
#' @param predict_args list of arguments passed to predict(model, ...)
#'   including interval will generate ribbon of the given type, see
#'   `r drc:::predict.drc` for more details, Default:
#'   `r list(interval = "prediction")`
#' @param aes_mapping ggplot2::aes(...) mapping columns of the model data
#'   and new data to the plot. Default:
#'   `r ggplot2::aes(x = log_dose, y = response)`
#' @param title character, plot title. Default: NULL
#' 
#' @export
plot_drc <- function(
    model, 
    newdata = NULL,
    predict_args = list(interval = "prediction"),
    aes_mapping = ggplot2::aes(
      x = log_dose,
      y = response),
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
  } else if (methods::is(newdata, "data.frame")) {
    assertthat::assert_that("log_dose" %in% names(newdata))
  } else {
    warning(
      paste0(
        "newdata should either be NULL or a data.frame, instead it is of ",
        "class ", class(newdata), "\n"))
  }
  
  assertthat::assert_that(methods::is(aes_mapping, "uneval"))
  assertthat::assert_that(
    is.null(title) || methods::is(title, "character"))
  
  predicted_values <- newdata |>
    dplyr::bind_cols(
      do.call(
        what = stats::predict,
        args = c(list(
          object = model,
          newdata = newdata),
          predict_args))) |>
    dplyr::rename(
      response = Prediction,
      response_lower = Lower,
      response_upper = Upper)

  plot <- ggplot2::ggplot(mapping = aes_mapping) +
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
