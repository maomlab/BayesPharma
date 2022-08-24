library(plyr)
library(tidyverse)


plate_map <- readr::read_tsv("intermediate_data/plate_map.tsv")

field_scores <- readr::read_csv("raw_data/NCATS_combos_20201115.csv") %>%
    dplyr::select(
        plate_id = PlateID,
        well_id = Image_Metadata_WellID,
        field_id = Image_Metadata_FieldID,
        nuclei_count = Image_Count_Nuclei,
        syn_nucs_count = Image_Count_syn_nucs) %>%
    dplyr::mutate(
        plate_id = ifelse(plate_id == "NFU005320-2", "NFU005320", plate_id)) %>%
    dplyr::left_join(
        plate_map,
        by = c("plate_id", "well_id"))  %>%
    dplyr::filter(!is.na(treatment_label))

# check that there are about
#   (n combos) * (plate replicates) * (drug1 doses) * (drug2 doses) * (fields per well)
#   6 * 5 * 6 * 6 * 16 = 17280 fields
nrow(field_scores)
# it looks like one frame is missing from well G14 on plate NFU005316 but otherwise ok

field_scores %>%
    readr::write_tsv("intermediate_data/field_scores.tsv")
