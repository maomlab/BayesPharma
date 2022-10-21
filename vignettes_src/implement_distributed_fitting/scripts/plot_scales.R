
library(ggplot2)
library(scales)

plate_map <- readr::read_tsv("intermediate_data/plate_map.tsv")


scales_x <- plate_map %>%
    dplyr::group_by(SampleID1) %>%
    dplyr::distinct(SampleID1_stoc_conc_in_uM) %>%
    dplyr::arrange(SampleID1_stoc_conc_in_uM) %>%
    dplyr::do({
        ggplot2::scale_x_continuous(
            name = .$Sample_id1,
            breaks = SampleID1_stoc_conc_in_uM %>% log10(),
            labels = SampleID1_stoc_conc_in_uM)
    })
    
