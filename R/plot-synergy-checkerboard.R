#' Plot Synergy Checkerboard
#'
#' @param data `data.frame` with columns the provided `treatment_1_variable`,
#'   `treatment_2_variable`, and `response_variable`
#' @param treatment_1_variable `character` column name for treatment 1
#' @param treatment_2_variable `character` column name for treatment 2
#' @param response_variable `character` column name for the response
#' @param treatment_1_label `character` used to make default title and axis
#'   labels
#' @param treatment_2_label `character` used to make default title and axis
#'   labels
#' @param treatment_1_units `character` used to make default axis labels
#' @param treatment_2_units `character` used to make default axis labels
#' @param plot_zero_dose `logical` plotted on the log scale, zero doses would be
#'     at `-Inf`, so to show them on the plot, add them as with a slight
#'     separation on the axis.
#' @param contour_color `character` the color of the contour lines
#'
#' @returns [ggplot2::ggplot] plot with light-blue to dark-blue tiles the dose
#'     response. Individual plot elements can be over-written and the
#'     plot can be saved with [ggplot2::ggsave()]
#'
#'
#' @importFrom rlang .data
#' @export
plot_synergy_checkerboard <- function(
  data,
  treatment_1_variable = "dose1",
  treatment_2_variable = "dose2",
  response_variable = "response",
  treatment_1_label = "Treatment 1",
  treatment_2_label = "Treatment 2",
  treatment_1_units = NULL,
  treatment_2_units = NULL,
  plot_zero_dose = TRUE,
  contour_color = "gold") {

  if (!(treatment_1_variable %in% names(data))) {
    warning(
      paste0(
        "The input data data.frame should have columns ['",
        treatment_1_variable, "', '", treatment_2_variable, "', '",
        response_variable, "'], but it is missing column '",
        treatment_1_variable, "'"))
  }

  if (!(treatment_2_variable %in% names(data))) {
    warning(
      paste0(
        "The input data data.frame should have columns ['",
        treatment_1_variable, "', '", treatment_2_variable, "', '",
        response_variable, "'], but it is missing column '",
        treatment_2_variable, "'"))
  }


  if (!(response_variable %in% names(data))) {
    warning(
      paste0(
        "The input data data.frame should have columns ['",
        treatment_1_variable, "', '", treatment_2_variable, "', '",
        response_variable, "'], but it is missing column '",
        response_variable, "'"))
  }


  d1 <- d1_label <- data[[treatment_1_variable]] |> unique() |> sort()
  d2 <- d2_label <- data[[treatment_2_variable]] |> unique() |> sort()
  if (plot_zero_dose) {
    if (d1[1] == 0) {
      d1[1] <- 10 ^ (log10(d1[2]) - 1.05 * (log10(d1[3]) - log10(d1[2])))
      data <- data |>
        dplyr::mutate(
          dose1 = ifelse(
            .data[[treatment_1_variable]] != 0,
            .data[[treatment_1_variable]],
            d1[1]))
    }
    if (d2[1] == 0) {
      d2[1] <- 10 ^ (log10(d2[2]) - 1.05 * (log10(d2[3]) - log10(d2[2])))
      data <- data |>
        dplyr::mutate(
          dose2 = ifelse(
            .data[[treatment_2_variable]] != 0,
            .data[[treatment_2_variable]],
            d2[1]))
    }
  }
  ggplot2::ggplot(data = data) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      panel.background =
        ggplot2::element_rect(fill = "grey40", colour = "grey40")) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(
        x = log10(.data[[treatment_1_variable]]),
        y = log10(.data[[treatment_2_variable]]),
        fill = .data[[response_variable]])) +
    ggplot2::geom_contour(
      mapping = ggplot2::aes(
        x = log10(.data[[treatment_1_variable]]),
        y = log10(.data[[treatment_2_variable]]),
        z = .data[[response_variable]]),
      color = contour_color) +
    ggplot2::coord_fixed() +
    ggplot2::ggtitle(
      paste0(
        "Treatment Combination: ",
        treatment_1_label, " vs. ",
        treatment_2_label)) +
    ggplot2::scale_x_continuous(
      paste(treatment_1_label, treatment_1_units),
      breaks = log10(d1),
      labels = signif(d1_label, 3),
      expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      paste(treatment_2_label, treatment_2_units),
      breaks = log10(d2),
      labels = signif(d2_label, 3),
      expand = c(0, 0)) +
    viridis::scale_fill_viridis(
      "Response",
      option = "cividis",
      guide = ggplot2::guide_colorbar(reverse = TRUE))
}
