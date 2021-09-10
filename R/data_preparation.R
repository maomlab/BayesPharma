#'
#'
#'
#'
#'
#'
#'

calc_log_dose <- function(data,
                          dose_col,
                          molar_concentration){
  data %>% dplyr::mutate(log_dose = dose_col * molar_concentration)
}
