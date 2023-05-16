#' Sigmoid Function
#'
#' Functional form for the sigmoid model.
#'
#' @param ac50 `numeric`, the `log_dose` of half maximal activity
#' @param hill `numeric`, slope of curve at the `ac50` units of
#'     response/log_dose
#' @param top `numeric`, value of maximal response for positive slope,
#'     this occurs when `log_dose = Inf`, with negative slope when
#'     `log_dose = -Inf`
#' @param bottom `numeric`, value of minimal response for positive
#'     slope, this occurs when log_dose = -Inf, with negative slope
#'     when log_dose = Inf
#' @param log_dose `numeric`, where to evaluate the the response where
#'     the log is base 10.
#'
#' @returns `numeric`, response given the `log_dose` and parameters
#'
#' @seealso [sigmoid_model]
#'
#' @examples
#' \dontrun{
#'  # Generate an agonist curve with an ec50 of 1 μM with the response
#'  # normalized to [0, 1] over the range 100 nM to 10 μM with half-log steps
#'  data <- data.frame(
#'    log_dose = seq(-7, -5, length.out = 5)) |>
#'      dplyr::mutate(
#'        response = stats::rnorm(
#'          n = length(log_dose),
#'          mean = BayesPharma::sigmoid(
#'            ac50 = -6,
#'            hill = 1,
#'            top = 1,
#'            bottom = 0,
#'            log_dose = log_dose),
#'        sd = .2))
#' }
#'
#' @export
sigmoid <- Vectorize(
  function(ac50, hill, top, bottom, log_dose) {
    bottom + (top - bottom) / (1 + 10 ^ ((ac50 - log_dose) * hill))
})


#' Convert Hill to Slope Parameters a the Sigmoid Functional Form
#'
#' The `hill` parameter controls the slope at the ac50 (`slope`) but
#' the `slope` also depends on the `top` and `bottom` parameters.
#' This helper function facilitates computing the `slope` from the
#' parameters.
#'
#' To prove the this function is correct, consider the derivative of the
#' response of the [sigmoid()] function with respect to the
#' `log_dose`:
#' \preformatted{
#'   d(response)/d(log_dose) =
#'     hill * log(10) * (top - bottom) * 10 ^ ((ac50 - log_dose) * hill) /
#'     ((10 ^ ((ac50 - log_dose) * hill) + 1) ^ 2)}
#' Evaluating this derivative at log_dose == ac50 simplifies the exponents:
#' \preformatted{
#'   slope = hill * log(10) * (top - bottom) / (1 + 1) ^ 2)
#'         = hill * log(10) * (top - bottom) / 4}
#'
#' @param hill `numeric` the hill coefficient for the [sigmoid()]
#'   function.
#' @param top `numeric` the top parameter for the [sigmoid()]
#'   function.
#' @param bottom `numeric` the bottom parameter for the
#'   [sigmoid()] function.
#'
#' @returns `numeric` the slope of the [sigmoid()] function at
#'   the ac50.
#'
#' @seealso [sigmoid()] [sigmoid_slope_to_hill()]
#'
#' @export
sigmoid_hill_to_slope <- function(hill, top, bottom) {
  hill * log(10) * (top - bottom) / 4
}

#' Convert Slope to Hill Parameters a the Sigmoid Functional Form
#'
#' The `hill` parameter controls the slope at the ac50 (`slope`) but
#' the `slope` also depends on the `top` and `bottom` parameters.
#' This helper function facilitates computing the `hill` from the
#' parameters.
#'
#' To prove the this function is correct, we will re-arrange the equation
#' relating the `slope` to the `hill` parameters
#' \preformatted{
#'   hill * log(10) * (top - bottom) / 4 = slope
#'   hill = slope * 4 / (log10 * (top - bottom))}
#'
#' @param slope `numeric` the slope of the [sigmoid()] at the
#'   ac50
#' @param top `numeric` the top parameter for the [sigmoid()]
#'   function.
#' @param bottom `numeric` the bottom parameter for the
#'   [sigmoid()] function.
#'
#' @returns `numeric` the hill coefficient of the [sigmoid()]
#'   function at.
#'
#' @seealso [sigmoid()] [sigmoid_hill_to_slope()]
#'
#' @export
sigmoid_slope_to_hill <- function(slope, top, bottom) {
  slope * 4 / (log(10) * (top - bottom))
}


#' Plot the Sigmoid Functional Form with Labeled Parameters
#'
#' Generates a plot of the [sigmoid()] functional form with the values
#' of the parameters `ac50`, `hill`, `top`, and `bottom`
#' labeled.
#'
#' @param ac50 `numeric` value for the `ac50` parameter, which is either `ec50`
#'   with the [sigmoid_agonist_formula()] or `ic50` for the
#'   [sigmoid_antagonist_formula()]
#' @param hill `numeric` value for the `hill` parameter
#' @param top `numeric` value for the `top` parameter
#' @param bottom `numeric` value for the `bottom` parameter
#' @param log_dose `numeric` vector of treatment values
#' @param treatment_units `character` units for the log dose
#' @param response_units `character` units for the response
#'
#' @returns [ggplot2::ggplot()] object
#'
#' @seealso [sigmoid()]
#'
#' @examples
#' \dontrun{
#'   plot_sigmoid_functional_form(
#'     ac50 = -6,
#'     hill = -1,
#'     top = 100,
#'     bottom = 0,
#'     treatment_label = "Log[Molar]",
#'     response_label = "% Baseline")}
#'
#' @importFrom rlang .data
#' @export
plot_sigmoid_functional_form <- function(
    ac50,
    hill,
    top,
    bottom,
    log_dose,
    treatment_units,
    response_units) {

  data <- data.frame(
    log_dose = log_dose,
    response = sigmoid(ac50, hill, top, bottom, log_dose))

  # midpoint of response
  y_m <- (top - bottom) / 2 + bottom

  # slope at ac50
  s <- sigmoid_hill_to_slope(hill, top, bottom)

  # rearrange y = m*x + b form for line
  b <- y_m - s * ac50

  # get x- and y-intercept values for the orange and blue lines
  xintercept <- b

  # nudge parameters to position labels
  nudge_x <- (max(log_dose) - min(log_dose)) / 20
  nudge_y <- (top - bottom) / 20

  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(
      yintercept = top,
      linetype = 2,
      size = 1.2,
      color = "darkgrey") +
    ggplot2::geom_hline(
      yintercept = bottom,
      linetype = 2,
      size = 1.2,
      color = "darkgrey") +
    ggplot2::geom_abline(
      intercept = xintercept,
      slope = s,
      color = "blue",
      size = 1.2) +
    ggplot2::geom_line(
      data = data.frame(
        x = c(ac50, ac50),
        y = c(top, bottom)),
      mapping = ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]]),
      size = 1.2,
      color = "orange") +
    ggplot2::geom_point(
      data = data.frame(
        x = ac50,
        y = y_m),
      mapping = ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]]),
      color = "blue",
      size = 1.5) +

    ggplot2::geom_line(
      data = data,
      mapping = ggplot2::aes(
        x = .data[["log_dose"]],
        y = .data[["response"]]),
      size = 1.2,
      color = "black") +

    ggplot2::geom_label(
      mapping = ggplot2::aes(
        y = .data[["top"]],
        x = max(.data[["log_dose"]]),
        label = paste0("top=", .data[["top"]])),
      nudge_y = - nudge_y,
      nudge_x = - nudge_x) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        x = .data[["ac50"]],
        y = .data[["bottom"]],
        label = paste0("ac50=", .data[["ac50"]])),
      nudge_y = nudge_y,
      nudge_x = nudge_x) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        y = .data[["bottom"]],
        x = min(.data[["log_dose"]]),
        label = paste0("bottom=", .data[["bottom"]])),
      nudge_y = nudge_y,
      nudge_x = nudge_x) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        x = .data[["ac50"]],
        y = .data[["y_m"]],
        label = paste0("hill=", .data[["hill"]])),
      nudge_y = nudge_y,
      nudge_x = nudge_x) +
    ggplot2::scale_x_continuous(
      treatment_units) +
    ggplot2::scale_y_continuous(
      response_units)
}
