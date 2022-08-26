



library(plyr)
library(tidyverse)
library(MPStats)


well_scores <- readr::read_tsv("intermediate_data/well_scores.tsv")


single_agent_well_scores <- dplyr::bind_rows(
    well_scores %>%
        dplyr::filter(sample_label_2 == "DMSO") %>%
        dplyr::mutate(
            compound = sample_label_1,
            log_dose = log10(dose1) - 9),
    well_scores %>%
        dplyr::filter(sample_label_1 == "DMSO") %>%
        dplyr::mutate(
            compound = sample_label_2,
            log_dose = log10(dose2) - 9)) %>%
    dplyr::mutate(
        is_control = FALSE,
        prob_positive = n_positive / count,
        cell_count = count) %>%
    dplyr::filter(compound != "DMSO")

fit_drc_score_by_dose <- function(well_scores){
  fits <- well_scores %>%
    plyr::ddply(c("compound"), function(curve_data){
      tryCatch({
#        weights <- 1/MPStats:::binomial_variance(curve_data$n_positive, curve_data$cell_count, prior_positive=10, prior_negative=10)
#        weights <- weights * nrow(curve_data)/sum(weights)
        fit <- drc::drm(
          formula=prob_positive ~ log_dose,
 #         weights=weights,
          data=curve_data,
          fct=drc::L.4(fixed=c(NA, NA, NA, NA)))
        log_dose <- seq(min(curve_data$log_dose), max(curve_data$log_dose), length.out=100)
        pred_value <- predict(fit, expand.grid(log_dose, 1))
        test_no_fit <- drc::noEffect(fit) %>% data.frame
        data.frame(log_dose, pred_value) %>%
          dplyr::mutate(
            slope=fit$coefficients[1],
            bottom=0,
            top=fit$coefficients[2],
            ic50=fit$coefficients[3],
            chi_squared_test = test_no_fit$.[1],
            degrees_of_freedom = test_no_fit$.[2],
            p_value = test_no_fit$.[3] %>% signif(2)) %>%
          return()
      }, error=function(e){
          cat("ERROR: Failed to fit curve for compound: ", curve_data$compound[1], "\n", sep="")
          cat("ERROR: ", e$message, "\n", sep="")
          return(data.frame())
      })
    })
}


fits <- single_agent_well_scores %>%
    fit_drc_score_by_dose()


poisson_quantile <- function(count, p){
  qgamma(p=p, shape=sum(count), rate=length(count))
}


plot_drc_score_by_dose <- function(well_scores, fits, subtitle=NULL){
  compound_dose_scores <- well_scores %>%
    dplyr::filter(!is_control) %>%
    dplyr::group_by(log_dose, compound) %>%
    dplyr::summarize(
      n_positive = sum(n_positive),
      cell_count = sum(cell_count),
      prob_positive = n_positive/cell_count,
      prob_positive_low = MPStats::binomial_quantile(n_positive, cell_count, .025),
      prob_positive_high = MPStats::binomial_quantile(n_positive, cell_count, .975)) %>%
    dplyr::ungroup()  
  # mean and 95% credible intervals for cell count by compound dose on the sqrt scale
  compound_cell_count <- well_scores %>%
    dplyr::filter(!is_control) %>%
    dplyr::group_by(log_dose, compound) %>%
    dplyr::summarize(
      mean = cell_count %>% mean %>% sqrt,
      low = poisson_quantile(cell_count, .025) %>% sqrt,
      high = poisson_quantile(cell_count, .975) %>% sqrt) %>%
    dplyr::ungroup()
  compound_cell_count_scale_factor <- compound_cell_count$high %>% max * 1/.13
  compound_cell_count <- compound_cell_count %>%
    dplyr::mutate(
      scaled_mean = mean / compound_cell_count_scale_factor,
      scaled_low = low / compound_cell_count_scale_factor,
      scaled_high = high / compound_cell_count_scale_factor)
  p <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    # cell counts
    ggplot2::geom_smooth(
      data=compound_cell_count,
      mapping=ggplot2::aes(
        x=log_dose,
        y=scaled_mean),
      color="green",
      size=1.5,
      method="loess",
      se=FALSE) +
    ggplot2::geom_errorbar(
      data=compound_cell_count,
      mapping=ggplot2::aes(
        x=log_dose,
        ymin=scaled_low,
        ymax=scaled_high),
      color="darkgreen") +
    ggplot2::geom_point(
      data=compound_cell_count,
      mapping=ggplot2::aes(
        x=log_dose,
        y=scaled_mean),
      color="darkgreen") +
    # scores
    ggplot2::geom_line(
      data=fits,
      mapping=ggplot2::aes(
        x=log_dose,
        y=pred_value),
      color="blue",
      size=1.5) +
    ggplot2::geom_errorbar(
      data=compound_dose_scores,
      mapping=ggplot2::aes(
        x=log_dose,
        ymin=prob_positive_low,
        ymax=prob_positive_high),
      color="darkblue") +
    ggplot2::geom_point(
      data=compound_dose_scores,
      mapping=ggplot2::aes(
        x=log_dose,
        y=prob_positive),
      color="darkblue") +
    # indicators
    geom_indicator(
      data=fits %>% dplyr::distinct(compound, p_value),
      mapping=ggplot2::aes(
        indicator=paste0("fit: ", p_value)),
      xpos="left",
      ypos="top",
      group=1) +
    ggplot2::ggtitle(
      label="Score by log dose",
      subtitle=subtitle) +
    ggplot2::scale_x_continuous(
      "log[Compound dose] (uM)") +
    ggplot2::scale_y_continuous(
      "Score",
      limits=c(0,.13),
      labels=scales::percent_format(),
      sec.axis = ggplot2::dup_axis(
        name = "Cell Count",
        breaks=c(0,10,20,30,40,50,60,70,80)/(compound_cell_count_scale_factor),
        labels=c(0,100,400,900,1600,2500,3600,4900,6400))) +
    ggplot2::facet_wrap(~compound, scales="free_x")
}



plot <- single_agent_well_scores %>%
    plot_drc_score_by_dose(
        fits = fits)

ggplot2::ggsave(
    filename = "product/figures/single_agent_dose_response_20201120.pdf",
    width = 6,
    heigh = 4)
ggplot2::ggsave(
    filename = "product/figures/single_agent_dose_response_20201120.png",
    width = 6,
    heigh = 4)
