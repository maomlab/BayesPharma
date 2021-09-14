#' Adds a log dose column to the tibble or data.frame of the data
#'
#'@param data tibble or data.frame
#'@param dose_col string object
#'@param molar_concentration numeric object
#'@return tibble or data.frame
#'
#'@export

calc_log_dose <- function(data,
                          dose_col,
                          molar_concentration){
  data %>% dplyr::mutate(log_dose = dose_col * molar_concentration)
}

