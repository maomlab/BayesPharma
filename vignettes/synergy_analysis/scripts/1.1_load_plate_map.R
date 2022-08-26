

library(plyr)
library(tidyverse)

treatment_labels <- readr::read_tsv("raw_data/treatment_labels.tsv")

plate_map <- dplyr::bind_rows(
    readr::read_csv(file = "raw_data/201023153422\ NFU005316.txt", skip = 2) %>%
        dplyr::mutate(plate_id = "NFU005316"),
    readr::read_csv(file = "raw_data/201023153422 NFU005317.txt", skip = 2) %>%
        dplyr::mutate(plate_id = "NFU005317"),
    readr::read_csv(file = "raw_data/201023153422 NFU005318.txt", skip = 2) %>%
        dplyr::mutate(plate_id = "NFU005318"),
    readr::read_csv(file = "raw_data/201023153422 NFU005319.txt", skip = 2) %>%
        dplyr::mutate(plate_id = "NFU005319"),
    readr::read_csv(file = "raw_data/201023153422 NFU005320.txt", skip = 2) %>%
        dplyr::mutate(plate_id = "NFU005320")) %>%
    dplyr::mutate(well_id = TWellID) %>%
    dplyr::mutate(
        row = well_id %>%
            stringr::str_extract("^[A-Z]") %>%
            purrr::map_int(~which(LETTERS == ., arr.ind = T)),
        column = well_id %>%
            stringr::str_extract("[0-9]+$") %>%
                as.integer()) %>%
    dplyr::left_join(
        treatment_labels %>%
        dplyr::rename(
            SampleID1 = SampleID,
            sample_label_1 = sample_label),
        by = "SampleID1") %>%
    dplyr::left_join(
        treatment_labels %>%
        dplyr::rename(
            SampleID2 = SampleID,
            sample_label_2 = sample_label),
        by = "SampleID2") %>%
    dplyr::mutate(
        drug_combo = paste0(sample_label_1, "_", sample_label_2),
        treatment_label_1 = ifelse(
            SampleID1 == "DMSO",
            "DMSO",
            paste0(sample_label_1, " ", SampleID1_stock_conc_in_uM, " nM")),
    treatment_label_2 = ifelse(
        SampleID2 == "DMSO",
        "DMSO",
        paste0(sample_label_2, " ", SampleID2_stock_conc_in_uM, " nM")),
    treatment_label = paste0(treatment_label_1, "\n", treatment_label_2))


plate_map %>%
  readr::write_tsv("intermediate_data/plate_map.tsv")

plate_layout <- plate_map %>%
    tidyr::pivot_wider(
        id_cols = c(plate_id, row),
        names_from = column,
        values_from = treatment_label)

plate_layout %>%
    readr::write_tsv("product/plate_layout.tsv")
