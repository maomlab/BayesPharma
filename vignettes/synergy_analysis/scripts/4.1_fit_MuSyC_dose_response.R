library(plyr)
library(tidyverse)
library(MPStats)
library(brms)


use_future <- FALSE

use_future <- TRUE
library(future)
Sys.setenv(DEBUGME = "batchtools")
library(batchtools)
library(future.batchtools)

future::plan(
    list(
        # for each drug combo
        tweak(
            future.batchtools::batchtools_slurm,
                resources = list(
                    account = "maom99",
                    ntasks=1,
                    ncpus=1L,
                    memory="10GB"),
            template = "../../inst/batchtools.greatlakes.tmpl"),
        tweak(
            future.batchtools::batchtools_slurm,
                resources = list(
                    account = "maom99",
                    ntasks=1,
                    ncpus=1L,
                    memory="2GB"),
            template = "../../inst/batchtools.greatlakes.tmpl")))


###############################
# fit the MuSyC synergy model #
###############################


well_scores <- readr::read_tsv("intermediate_data/well_scores.tsv") %>%
    dplyr::mutate(
        dose1_nM = dose1,
        dose2_nM = dose2,
        dose1 = dose1_nM * 1e-9,
        dose2 = dose2_nM * 1e-9,
        d1_scale_factor = 1e-6,
        d2_scale_factor = 1e-6)

devtools::load_all()
synergy_model_v12 <- well_scores %>%
    dplyr::filter(drug_combo == "GS-441524_Nitazoxanide") %>%
    MPStats::fit_MuSyC_score_by_dose_robust(
        group_vars = vars(drug_combo),
        stan_model_args = list(verbose = TRUE),
        #model_evaluation_criteria = NULL,
        open_progress = FALSE,
        silent = FALSE,
        future = use_future)

save(synergy_model_v10, file = "intermediate_data/synergy_model_v10.Rdata")
load("intermediate_data/synergy_model_v10.Rdata")




estimated_parameters <- synergy_model_v10 %>%
    dplyr::rowwise() %>%
    dplyr::do({
        combo_model <- .
        combo_model$model %>%
            tidybayes::spread_draws(
                E0,
                E1, C1, s1,
                E2, C2, s2,
                E3, alpha) %>%
            tidybayes::median_qi() %>%
            dplyr::mutate(
                drug_combo = combo_model$drug_combo)
    }) %>%
    dplyr::ungroup()
estimated_parameters %>%
    dplyr::transmute(
        drug_combo,
        E0 = E0 * 100,
        C1, E1 = E1 * 100, s1,
        C2, E2 = E2 * 100, s2,
        E3 = E3 * 100,
        alpha) %>%
    data.frame




estimated_parameters %>%
    readr::write_tsv("product/estimated_parameters_20201127.tsv")



