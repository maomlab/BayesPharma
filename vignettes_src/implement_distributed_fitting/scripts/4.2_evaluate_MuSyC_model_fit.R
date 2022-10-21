
library(plyr)
library(tidyverse)
library(shinystan)
library(MPStats)
library(bayesplot)
library(tidybayes)
library(rstan)
library(brms)


well_scores <- readr::read_tsv("intermediate_data/well_scores.tsv") %>%
    dplyr::mutate(
        dose1_nM = dose1,
        dose2_nM = dose2,
        dose1 = dose1_nM * 1e-9,
        dose2 = dose2_nM * 1e-9,
        d1_scale_factor = 1e-6,
        d2_scale_factor = 1e-6)


#############################
# Analyze the fitted models #
#############################

# interactively look for problems with the model fit
synergy_model_v10$model[[5]] %>%
    shinystan::launch_shinystan()


########################
# LOO Cross Validation #
########################

models <- synergy_model_v10$model %>%
    purrr::map(function(model) {
        cat("Computing loo \n", sep = "")
        model %>% brms::add_criterion(
                criterion = "loo",
                reloo = TRUE,
                cores = 4)
     })


#############################################
# prior vs posterior marginal distributions #
#############################################

devtools::load_all()
synergy_model_v12_prior <- well_scores %>%
    dplyr::filter(drug_combo == "GS-441524_Nitazoxanide") %>%
    MPStats::fit_MuSyC_score_by_dose_robust(
        group_vars = dplyr::vars(drug_combo),
        stan_model_args = list(verbose = TRUE),
        model_evaluation_criteria = NULL,
        open_progress = FALSE,
        silent = FALSE,
        sample_prior = "only",
        future = FALSE)

prior_draws <- synergy_model_v12_prior %>%
    dplyr::rowwise() %>%
    dplyr::do({
        combo_model <- .
        combo_model$model %>%
            tidybayes::spread_draws(
                b_logE0_Intercept,
                b_logE1_Intercept, b_logC1_Intercept, b_h1_Intercept,
                b_logE2_Intercept, b_logC2_Intercept, b_h2_Intercept,                
                b_logE3_Intercept, b_logalpha_Intercept)
    }) %>%
    tidyr::pivot_longer(
        cols = -tidyselect::starts_with("."),
        names_to = "parameter",
        values_to = "value")    

posterior_draws <- synergy_model_v11 %>%
    dplyr::rowwise() %>%
    dplyr::do({
        combo_model <- .
        combo_model$model %>%
            tidybayes::spread_draws(
                b_logE0_Intercept,
                b_logE1_Intercept, b_logC1_Intercept, b_h1_Intercept,
                b_logE2_Intercept, b_logC2_Intercept, b_h2_Intercept,                
                b_logE3_Intercept, b_logalpha_Intercept)
    }) %>%
    tidyr::pivot_longer(
        cols = -tidyselect::starts_with("."),
        names_to = "parameter",
        values_to = "value")

data <- dplyr::bind_rows(
    prior_draws %>% dplyr::mutate(type = "Prior"),
    posterior_draws %>% dplyr::mutate(type = "Posterior"))

ggplot2::ggplot(data = data) +
    ggplot2::theme_bw() +
    ggplot2::geom_density(
        mapping = ggplot2::aes(
            x = value,
            fill = type),
        color = "black",
        size = .3,
        alpha = .7) +
    ggplot2::facet_wrap(
        facets = dplyr::vars(parameter),
        scales = "free")
    
ggplot2::ggsave(
    filename = "product/figures/MuSyC_prior_posterior_marginal_distributions.pdf",
    width = 8, height = 5)


#
# Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., Bürkner, P. (2019).
# Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC.
#

data <- posterior_draws %>%
    dplyr::filter(.iteration > 2000) %>%
    dplyr::mutate(.chain = as.factor(.chain)) %>%
    dplyr::group_by(parameter, .chain) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()

ggplot2::ggplot(data = data) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(
        mapping = ggplot2::aes(
            x = rank,
            y = value,
            group = .chain,
            color = .chain)) +
    ggplot2::facet_wrap(
        facets = dplyr::vars(parameter),
        scales = "free") +
    ggplot2::scale_color_discrete(
        "Chain")

ggplot2::ggsave(
    filename = "product/figures/MuSyC_prosterior_rank_by_chain.pdf",
    width = 8, height = 5)

###############################
# posterior predictive checks #
###############################

# https://www.rdocumentation.org/packages/bayesplot/versions/0.0.12/topics/PPC-overview

model <- synergy_model_v10$model[[1]]
model %>% brms::expose_functions(vectorize = TRUE)

model %>% brms::pp_check(type='error_scatter_avg')
model %>% brms::pp_check(type = "stat_grouped", stat = "std", group = "plate_id")


model %>% brms::pp_check()

model %>% brms::pp_check(type = "error_hist", nsamples = 11)

model %>% brms::pp_check(type = "scatter_avg", nsamples = 100)

model %>% brms::pp_check(type = "loo_pit")

###########
# can the error be explained by batch effects?

z <- model %>% brms::pp_check(type='error_scatter_avg')

pp_data <- z$data %>%
    dplyr::bind_cols(
        well_scores %>%
        dplyr::filter(drug_combo == "GS-441524_Tizoxanide")) %>%
    dplyr::mutate(
        rel_error = (n_positive - avg_error)/n_positive)

# average error by n_positive
plot <- ggplot2::ggplot(data = pp_data) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.15, 0.75)) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = log10(n_positive + 1),
            y = avg_error,
            fill = plate_id,
            color = plate_id),
        size = 1,
        alpha = .5) +
    ggplot2::geom_smooth(
        mapping = ggplot2::aes(
            x = log10(n_positive + 1),
            y = avg_error,
            color = plate_id,
            group = plate_id),
        se=FALSE) +
    ggplot2::scale_y_continuous(
        "Average MuSyC model error") +
    ggplot2::scale_x_continuous(
        "Infected cells per well",
        breaks = log10(c(0, 3, 10, 30, 100, 300, 1000)+1),
        labels = c("0", "3", "10", "30", "100", "300", "1000")) +
    ggplot2::ggtitle(
        label = "Average MuSyC model error by number of infected cells per well") 

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_error_by_n_positive_plate_id_20201130.pdf",
    width = 6,
    height = 4,
    useDingbats=FALSE)

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_error_by_n_positive_plate_id_20201130.png",
    width = 6,
    height = 4)

# relative error by n_positive
plot <- ggplot2::ggplot(data = pp_data) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.15, 0.75)) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = log10(n_positive + 1),
            y = log(rel_error),
            fill = plate_id,
            color = plate_id),
        size = 1,
        alpha = .5) +
    ggplot2::geom_smooth(
        mapping = ggplot2::aes(
            x = log10(n_positive + 1),
            y = log(rel_error),
            color = plate_id,
            group = plate_id),
        se=FALSE) +
    ggplot2::scale_y_continuous(
        "Average MuSyC model relative error") +
    ggplot2::scale_x_continuous(
        "Infected cells per well",
        breaks = log10(c(0, 3, 10, 30, 100, 300, 1000)+1),
        labels = c("0", "3", "10", "30", "100", "300", "1000")) +
    ggplot2::ggtitle(
        label = "Average MuSyC model error by number of infected cells per well") 

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_rel_error_by_n_positive_plate_id_20201130.pdf",
    width = 6,
    height = 4,
    useDingbats=FALSE)

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_rel_error_by_n_positive_plate_id_20201130.png",
    width = 6,
    height = 4)

# error by count
plot <- ggplot2::ggplot(data = pp_data) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.15, 0.75)) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = log10(count),
            y = avg_error,
            fill = plate_id,
            color = plate_id),
        size = 1,
        alpha = .5) +
    ggplot2::geom_smooth(
        mapping = ggplot2::aes(
            x = log10(count),
            y = avg_error,
            color = plate_id,
            group = plate_id),
        se=FALSE) +
    ggplot2::scale_y_continuous(
        "Average MuSyC model error") +
    ggplot2::scale_x_continuous(
        "Cell count per well",
        limits = log10(c(1000, 10800)),
        breaks = log10(c(1000, 2500, 5000, 10000)),
        labels = c("1k", "2.5k", "5k", "10k")) +
    ggplot2::ggtitle(
        label = "Average MuSyC model error by number of cells per well") 

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_error_by_count_plate_id_20201130.pdf",
    width = 6,
    height = 4,
    useDingbats=FALSE)

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_error_by_count_plate_id_20201130.png",
    width = 6,
    height = 4)


# error by score
plot <- ggplot2::ggplot(data = pp_data) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.15, 0.75)) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = score,
            y = avg_error,
            fill = plate_id,
            color = plate_id),
        size = 1,
        alpha = .5) +
    ggplot2::geom_smooth(
        mapping = ggplot2::aes(
            x = score,
            y = avg_error,
            color = plate_id,
            group = plate_id),
        se=FALSE) +
    ggplot2::scale_y_continuous(
        "Average MuSyC model error") +
    ggplot2::scale_x_continuous(
        "% infected cells per well",
        labels = scales::percent_format()) +
    ggplot2::ggtitle(
        label = "Average MuSyC model error by % infected cells per well") 

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_error_by_score_plate_id_20201130.pdf",
    width = 6,
    height = 4,
    useDingbats=FALSE)

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_average_error_by_score_plate_id_20201130.png",
    width = 6,
    height = 4)



# error n_positive vs count side by side
data <- dplyr::bind_rows(
    pp_data %>%
    dplyr::mutate(type = "MuSyC Prediction") %>%
    dplyr::transmute(
        type,
        plate_id,
        count,
        n_positive = n_positive - avg_error),
    pp_data %>%
    dplyr::mutate(type = "Observed Data") %>%
    dplyr::select(
        type,
        plate_id,
        count,
        n_positive))

plot <- ggplot2::ggplot(data = data) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = c(0.08, 0.3)) +
    ggplot2::geom_point(
        mapping = ggplot2::aes(
            x = log10(count),
            y = log10(n_positive+1),
            fill = plate_id,
            color = plate_id),
        size = 1,
        alpha = .5) +
    ggplot2::geom_smooth(
        mapping = ggplot2::aes(
            x = log10(count),
            y = log10(n_positive+1),
            color = plate_id,
            group = plate_id),
        se = FALSE) +
    ggplot2::facet_wrap(facets = dplyr::vars(type), ncol = 2) +
    ggplot2::scale_x_continuous(
        "Cell count per well",
        limits = log10(c(1000, 10800)),
        breaks = log10(c(1000, 2500, 5000, 10000)),
        labels = c("1k", "2.5k", "5k", "10k")) +
    ggplot2::scale_y_continuous(
        "Infected cells per well",
        breaks = log10(c(0, 3, 10, 30, 100, 300, 1000)+1),
        labels = c("0", "3", "10", "30", "100", "300", "1000")) +
    ggplot2::ggtitle(
        label = "Predicted vs observed infected cells by cells per well")

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_n_pos_vs_count_pred_vs_data_plate_id_20201130.pdf",
    width = 9,
    height = 4,
    useDingbats=FALSE)

ggplot2::ggsave(
    filename = "product/figures/batch_effects/MuSyC_n_pos_vs_count_pred_vs_data_plate_id_20201130.png",
    width = 9,
    height = 4)




############
# PSIS-LOO #
############
# https://mc-stan.org/loo/articles/loo2-example.html

# Vehtari, A., Gelman, A., and Gabry, J. (2017). Practical Bayesian
# model evaluation using leave-one-out cross-validation and
# WAIC. Statistics and Computing. 27(5),
# 1413–1432. :10.1007/s11222-016-9696-4. Links: published | arXiv
# preprint.

# Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry,
# J. (2019). Pareto smoothed importance sampling. arXiv preprint
# arXiv:1507.04544.


model <- model %>% brms::add_criterion(
    criterion = "loo",
    reloo = TRUE)

model$criteria$loo
# check that
#   1) no Pareto k estimates are ok (k < 0.7)
#   2) p_loo < number of parameters

# Marginal posterior predictive checks
# LOO-PIT values are cumulative probabilities for y_i computed using
# the LOO marginal predictive distributions 𝑝(y_i|y_{-i}). For a good
# model, the distribution of LOO-PIT values should be uniform

model %>% pp_check(type = "loo_pit_overlay")

yrep <- model %>% brms::posterior_predict()


# to run this with brms
bayesplot::ppc_loo_pit_overlay(
    y = well_scores %>%
        dplyr::filter(drug_combo == "GS-441524_Nitazoxanide") %>%
        magrittr::extract2("score"),
    yrep = yrep,
    lw = weights(model$citeria$loo$psis_object))


#############
# residuals #
#############

# https://mjskay.github.io/tidybayes/articles/tidybayes-residuals.html
z <- well_scores %>%
    tidybayes::add_residual_draws(
        synergy_model_v11$model[[1]])
