#' Sigmoid Function
#'
#' Functional form for the sigmoid model.
#'
#' @param ac50 numeric, the log_dose of half maximal activity
#' @param hill numeric, slope of curve at the ac50 units of
#'     response/log_dose
#' @param top numeric, value of maximal response for positive slope,
#'     this occurs when log_dose = Inf, with negative slope when
#'     log_dose = -Inf
#' @param bottom numeric, value of minimal response for positive
#'     slope, this occurs when log_dose = -Inf, with negative slope
#'     when log_dose = Inf
#' @param log_dose numeric, where to evaluate the the response where
#'     the log is base 10.
#' @returns numeric, response given the log_dose and parameters
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


#' For the sigmoid functional form, convert hill to slope parameter
#'
#' The \code{hill} parameter controls the slope at the ac50 (\code{slope}) but
#' the \code{slope} also depends on the \code{top} and \code{bottom} parameters.
#' This helper function facilitates computing the \code{slope} from the 
#' parameters.
#'
#' To prove the this function is correct, consider the derivative of the
#' response of the \code{\link{sigmoid}} function with respect to the
#' \code{log_dose}:
#' \preformatted{
#'   d(response)/d(log_dose) =
#'     hill * log(10) * (top - bottom) * 10 ^ ((ac50 - log_dose) * hill) /
#'     ((10 ^ ((ac50 - log_dose) * hill) + 1) ^ 2)}
#' Evaluating this derivative at log_dose == ac50 simplifies the exponents:
#' \preformatted{
#'   slope = hill * log(10) * (top - bottom) / (1 + 1) ^ 2)
#'         = hill * log(10) * (top - bottom) / 4}
#'
#' @param hill \code{numeric} the hill coefficient for the \code{\link{sigmoid}}
#'   function.
#' @param top \code{numeric} the top parameter for the \code{\link{sigmoid}}
#'   function.
#' @param bottom \code{numeric} the bottom parameter for the
#'   \code{\link{sigmoid}} function.
#'   
#' @returns \code{numeric} the slope of the \code{\link{sigmoid}} function at
#'   the ac50.
#'
#' @seealso \code{\link{sigmoid}} \code{\link{sigmoid_slope_to_hill}}
#' 
#' @export
sigmoid_hill_to_slope <- function(hill, top, bottom){
  hill * log(10) * (top - bottom) / 4
}
  
#' For the sigmoid functional form, convert slope to the hill parameter
#'
#' The \code{hill} parameter controls the slope at the ac50 (\code{slope}) but
#' the \code{slope} also depends on the \code{top} and \code{bottom} parameters.
#' This helper function facilitates computing the \code{hill} from the 
#' parameters.
#'
#' To prove the this function is correct, we will re-arrange the equation
#' relating the \code{slope} to the \code{hill} parameters
#' \preformatted{
#'   hill * log(10) * (top - bottom) / 4 = slope
#'   hill = slope * 4 / (log10 * (top - bottom))}
#'
#' @param slope \code{numeric} the slope of the \code{\link{sigmoid}} at the
#'   ac50
#' @param top \code{numeric} the top parameter for the \code{\link{sigmoid}}
#'   function.
#' @param bottom \code{numeric} the bottom parameter for the
#'   \code{\link{sigmoid}} function.
#'   
#' @returns \code{numeric} the hill coefficient of the \code{\link{sigmoid}}
#'   function at.
#'
#' @seealso \code{\link{sigmoid}} \code{\link{sigmoid_hill_to_slope}}
#' 
#' @export
sigmoid_slope_to_hill <- function(slope, top, bottom){
  slope * 4 / (log(10) * (top - bottom))
}


#' Plot the sigmoid functional form with labeled parameters
#' 
#' Generates a plot of the \code{\link{sigmoid}} functional form with the values
#' of the parameters \code{ac50}, \code{hill}, \code{top}, and \code{bottom}
#' labeled.
#' @param ac50 \code{numeric} value for the \code{ac50} parameter, which is
#'   either the \code{ec50} for the \code{\link{sigmoid_agonist_model}} or the
#'   \code{ic50} for the \code{\link{sigmoid_antagoinst_model}}.
#' @param hill \code{numeric} value for the \code{hill} parameter.
#' @param top \code{numeric} value for the \code{top} parameter. 
#' @param bottom \code{numeric} value for the \code{bottom} parameter.
#' 
#' @returns \code{\link[ggplot2]{ggplot}} object
#' 
#' @seealso \code{\link{sigmoid}}
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

  # midpoint of repsonse
  y_m = (top-bottom) / 2 + bottom
  
  # slope at ac50
  s = sigmoid_hill_to_slope(hill, top, bottom)
  
  # rearrange y = m*x + b form for line
  b = y_m - s * ac50
  
  # get x- and y-intercept values for the orange and blue lines
  xintercept = b
  yintercept = b / hill
  
  # nudge parameters to position labels
  nudge_x = (max(log_dose) - min(log_dose)) / 20
  nudge_y = (top - bottom) / 20
  
  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(
      yintercept = top,
      linetype = 2,
      size = 1.2,
      color = "darkgrey")+
    ggplot2::geom_hline(
      yintercept = bottom,
      linetype = 2,
      size = 1.2,
      color = "darkgrey")+
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
        x = x,
        y = y),
      size = 1.2,
      color = "orange") +
    ggplot2::geom_point(
      data = data.frame(
        x = ac50,
        y = y_m),
      mapping = ggplot2::aes(
        x = x,
        y = y),
      color = "blue",
      size = 1.5) +

    ggplot2::geom_line(
      data = data,
      mapping = ggplot2::aes(
        x = log_dose,
        y = response),
      size = 1.2,
      color = "black") +

    ggplot2::geom_label(
      mapping = ggplot2::aes(
        y = top,
        x = max(log_dose),
        label = paste0("top=", top)),
      nudge_y = - nudge_y,
      nudge_x = - nudge_x) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        x = ac50,
        y = bottom,
        label = paste0("ac50=", ac50)),
      nudge_y = nudge_y,
      nudge_x = nudge_x) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        y = bottom,
        x = min(log_dose),
        label = paste0("bottom=", bottom)),
      nudge_y = nudge_y,
      nudge_x = nudge_x) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(
        x = ac50,
        y = y_m,
        label = paste0("hill=", hill)),
      nudge_y = nudge_y,
      nudge_x = nudge_x) +
    ggplot2::scale_x_continuous(
      treatment_units) +
    ggplot2::scale_y_continuous(
      response_units)
}
