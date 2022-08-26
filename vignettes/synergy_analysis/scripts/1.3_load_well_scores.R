

library(plyr)
library(tidyverse)

field_scores <- readr::read_tsv("intermediate_data/field_scores.tsv")


##########################################
# Aggregate field data to the well level #
##########################################
# one trick thing is that DMSO controls and
# single drug data can be re-used for different
# combo screens.

well_scores <- field_scores %>%
    # assign zero and single agent treatments to the combo treatments
    dplyr::filter(SampleID1 != "DMSO", SampleID2 != "DMSO") %>%
    dplyr::group_by(SampleID1, SampleID2) %>%
    dplyr::do({
        data <- .
        DMSO_treatments <- field_scores %>%
            dplyr::filter(SampleID1 == "DMSO", SampleID2 == "DMSO") %>%
            dplyr::mutate(
                SampleID1_stock_conc_in_uM = 0,
                SampleID2_stock_conc_in_uM = 0,
                drug_combo = data$drug_combo[1])
         drug1_treatments <- field_scores %>%
                dplyr::filter(
                    SampleID1 == "DMSO",
                    SampleID2 == data$SampleID2[1]) %>%
                dplyr::mutate(
                    SampleID1_stock_conc_in_uM = 0,
                    drug_combo = data$drug_combo[1])
         drug2_treatments <- field_scores %>%
                dplyr::filter(
                    SampleID1 == data$SampleID1[1],
                    SampleID2 == "DMSO") %>%
                dplyr::mutate(
                    SampleID2_stock_conc_in_uM = 0,
                    drug_combo = data$drug_combo[1])
        combo_treatments <- data
        cat(data$drug_combo[1], "\n")
        cat("  n DMSO treatments: ", nrow(DMSO_treatments) / 16, "\n", sep = "")
        cat("  n drug1 treatments: ", nrow(drug1_treatments) / 16, "\n", sep = "")
        cat("  n drug2 treatments: ", nrow(drug2_treatments) / 16, "\n", sep = "")
        cat("  n combo treatments: ", nrow(combo_treatments) / 16, "\n", sep = "")        
        dplyr::bind_rows(
            DMSO_treatments,
            drug1_treatments,
            drug2_treatments,
            combo_treatments)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
        dose1 = SampleID1_stock_conc_in_uM,
        dose2 = SampleID2_stock_conc_in_uM) %>%
    dplyr::group_by(
        plate_id,
        well_id,
        SampleID1, SampleID2,
        sample_label_1, sample_label_2,
        treatment_label_1, treatment_label_2,
        treatment_label,
        drug_combo,
        dose1, dose2) %>%
    dplyr::summarize(
        n_positive = sum(syn_nucs_count),
        count = sum(nuclei_count),
        score = n_positive / count,
        .groups = "drop")



#check per combo number of wells
#   combo: (plate replicates) * (drug1 doses) * (drug2 doses) +
#   drug1: (plate_replicates) * (drug1_doses) * 3
#   drug2: (plate_replicates) * (drug2_doses) * 2
#   DMSO:  (plate_replicates) * (n combos)
#
#   5*5*5 + 5*5*3 + 5*5*2 + 5*6 = 280
well_scores %>% dplyr::count(drug_combo)


well_scores %>%
    readr::write_tsv("intermediate_data/well_scores.tsv")
