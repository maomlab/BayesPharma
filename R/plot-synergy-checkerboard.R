#' Plot synergy checkerboard
#'
#' @param data data.frame with columns [dose1, dose2, response]
#' @param treatment_1_label used to make default title and axis labels
#' @param treatment_2_label used to make default title and axis labels
#' @param treatment_1_units used to make default axis labels
#' @param treatment_2_units used to make default axis labels
#' @param plot_zero_dose plotted on the log scale, zero doses would be
#'     at -Inf, so to show them on the plot, add them as with a slight
#'     separation on the axis.
#' @param contour_color default 'gold'
#'
#' @return ggplot2 plot with light-blue to dark-blue tiles the dose
#'     response. Individual plot elements can be over-written and the
#'     plot can be saved with `ggplot2::ggsave()`
#'
#' @export
plot_synergy_checkerboard <- function(
    data,
    treatment_1_label = "Treatment 1",
    treatment_2_label = "Treatment 2",
    treatment_1_units = NULL,
    treatment_2_units = NULL,
    plot_zero_dose = TRUE,
    contour_color = "gold") {

  d1 <- d1_label <- data$dose1 |> unique() |> sort()
  d2 <- d2_label <- data$dose2 |> unique() |> sort()
  if (plot_zero_dose) {
    if (d1[1] == 0) {
      d1[1] <- 10 ^ (log10(d1[2]) - 1.05 * (log10(d1[3]) - log10(d1[2])))
      data <- data |>
        dplyr::mutate(dose1 = ifelse(dose1 != 0, dose1, d1[1]))
    }
    if (d2[1] == 0) {
      d2[1] <- 10 ^ (log10(d2[2]) - 1.05 * (log10(d2[3]) - log10(d2[2])))
      data <- data |>
        dplyr::mutate(dose2 = ifelse(dose2 != 0, dose2, d2[1]))
    }
  }
  ggplot2::ggplot(data = data) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.background =
        ggplot2::element_rect(fill = "grey40", colour = "grey40")) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(
        x = log10(dose1),
        y = log10(dose2),
        fill = response)) +
    ggplot2::geom_contour(
      mapping = ggplot2::aes(
        x = log10(dose1),
        y = log10(dose2),
        z = response),
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
      guide = guide_colorbar(reverse = TRUE))
}
