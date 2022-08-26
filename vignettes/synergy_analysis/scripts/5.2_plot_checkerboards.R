

library(plyr)
library(tidyverse)
library(MPStats)
library(viridis)

well_scores <- readr::read_tsv("intermediate_data/well_scores.Rdata")

treatment_scores <- well_scores %>%
    dplyr::group_by(drug_combo, dose1, dose2) %>%
    dplyr::summarize(
        score = median(n_positive / count),
        count = median(sqrt(count))^2,
        .groups = "drop")


#########################
# plot percent infected #
#########################
treatment_scores %>%
    dplyr::group_by(drug_combo) %>%
    dplyr::do({
        data <- .
        drug_combo <- data$drug_combo[1]
        drug1 <- drug_combo %>% stringr::str_replace("_.+$", "")
        drug2 <- drug_combo %>% stringr::str_replace("^.+_", "")
        plot <- MPStats::plot_checkerboard_score_by_dose(
            treatment_scores = data,
            treatment_1_label = drug1,
            treatment_2_label = drug2,
            treatment_1_units = "nM",
            treatment_2_units = "nM") +
            viridis::scale_fill_viridis(
                "% Infected",
                limits = c(0, .125),
                breaks = c(.0, .05, .1),
                labels = scales::percent(c(0, .05, .1)),
                guide = guide_colorbar(reverse = TRUE),
                option = "cividis")
        ggplot2::ggsave(
            paste0("product/figures/checkerboard_score_", data$drug_combo[1], ".pdf"),
            width = 6,
            height = 6)
        ggplot2::ggsave(
            paste0("product/figures/checkerboard_score_", data$drug_combo[1], ".png"),
            width = 6,
            height = 6)
        data.frame()
        })

###################
# plot Cell Count #
###################
treatment_scores %>%
    dplyr::group_by(drug_combo) %>%
    dplyr::do({
        data <- .
        data <- data %>% dplyr::mutate(score = count)
        drug_combo <- data$drug_combo[1]
        drug1 <- drug_combo %>% stringr::str_replace("_.+$", "")
        drug2 <- drug_combo %>% stringr::str_replace("^.+_", "")
        plot <- MPStats::plot_checkerboard_score_by_dose(
            treatment_scores = data,
            treatment_1_label = drug1,
            treatment_2_label = drug2,
            treatment_1_units = "nM",
            treatment_2_units = "nM",
            contour_color = "black") +
            viridis::scale_fill_viridis(
                "Cell Count",
                limits = c(0, 6500),
                breaks = c(0, 2000, 4000, 6000),
                labels = c("0", "2k", "4k", "6k"),
                guide = guide_colorbar(reverse = TRUE),
                option = "B")
        ggplot2::ggsave(
            paste0("product/figures/checkerboard_cell_count_", data$drug_combo[1], ".pdf"),
            width = 6,
            height = 6)
        ggplot2::ggsave(
            paste0("product/figures/checkerboard_cell_count_", data$drug_combo[1], ".png"),
            width = 6,
            height = 6)
        data.frame()
        })

########################################
# plot MuSyC fits on the checkerboards #
########################################

estimated_parameters <- readr::read_tsv("product/estimated_parameters_20201118.tsv") %>%
    dplyr::mutate(
        drug_combo = c(
            "EIDD-1931_Nitazoxanide",
            "EIDD-1931_Tizoxanide",
            "GS-441524_Nitazoxanide",
            "GS-441524_Tizoxanide",
            "Remdesivir_Nitazoxanide",
            "Remdesivir_Tizoxanide"))


fit_treatment_scores <- well_scores %>%
    dplyr::group_by(drug_combo, dose1, dose2) %>%
    dplyr::summarize(
        score = median(n_positive / count),
        .groups = "drop") %>%
    dplyr::group_by(drug_combo) %>%
    dplyr::mutate(
        d1_scale_factor = max(dose1),
        d2_scale_factor = max(dose2)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
        estimated_parameters,
        by = c("drug_combo")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        fitted_score = MPStats::generate_MuSyC_effects(
            d1 = dose1 / d1_scale_factor,
            d2 = dose2 / d2_scale_factor,
            E0 = E0,
            C1 = C1, E1 = E1, s1 = s1,
            C2 = C2, E2 = E2, s2 = s2,
            alpha = alpha, E3 = E3)) %>%
    dplyr::ungroup()



# Plot fitted contours
fit_treatment_scores %>%
    dplyr::group_by(drug_combo) %>%
    dplyr::do({
        data <- .
        drug_combo <- data$drug_combo[1]
        drug1 <- drug_combo %>% stringr::str_replace("_.+$", "")
        drug2 <- drug_combo %>% stringr::str_replace("^.+_", "")
        plot <- plot_checkerboard_score_by_dose(
            treatment_scores = data,
            treatment_1_label = drug1,
            treatment_2_label = drug2,
            treatment_1_units = "nM",
            treatment_2_units = "nM") +
            ggplot2::geom_contour(
                mapping = ggplot2::aes(
                    x = log10(dose1),
                    y = log10(dose2),
                    z = fitted_score),
                color = "purple",
                size = 2) +
            viridis::scale_fill_viridis(
                "% Infected",
                limits = c(0, .125),
                breaks = c(.0, .05, .1),
                labels = scales::percent(c(0, .05, .1)),
                guide = guide_colorbar(reverse = TRUE),
                option = "cividis")
        ggplot2::ggsave(
            paste0("product/figures/checkerboard_fitted_", data$drug_combo[1], ".pdf"),
            width = 6,
            height = 6)
        ggplot2::ggsave(
            paste0("product/figures/checkerboard_fitted_", data$drug_combo[1], ".png"),
            width = 6,
            height = 6)
        data.frame()
        })
