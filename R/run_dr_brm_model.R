#' title
#'
#'@param data
#'@param priors
#'@param inits
#'@param iter
#'@param warmup
#'@param chains
#'@param adapt_delta
#'@param max_treedepth
#'
#'@return brmsfit
#'
#'@export

single_drug_model <- function(data,
                              response_col_name,
                              log_dose_col_name,
                              priors = NULL,
                              inits = 0,
                              iter = 8000,
                              warmup = 4000,
                              chains = 4,
                              sample_prior = "no",
                              adapt_delta = 0.99,
                              max_treedepth = 10){

  input_data <- data %>%
    dplyr::rename(response = response_col_name) %>%
    dplyr::rename(log_dose = log_dose_col_name)

  brms::brm(
    formula = brms::brmsformula(
      response ~ (bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))),
      ec50 + hill + top + bottom ~ 1, nl = TRUE),
    data = input_data,
    prior = priors,
    inits = inits,
    iter = iter,
    warmup = warmup,
    sample_prior = sample_prior,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
  )
  }

#' title
#'
#'@param data
#'@param priors
#'@param inits
#'@param iter
#'@param warmup
#'@param chains
#'@param adapt_delta
#'@param max_treedepth
#'
#'@return brmsfit
#'
#'@export

multi_drug_model <- function(data,
                             response_col_name,
                             log_dose_col_name,
                             drug_col_name,
                             priors = NULL,
                             inits = 0,
                             iter = 8000,
                             warmup = 4000,
                             chains = 4,
                             sample_prior = "no",
                             adapt_delta = 0.99,
                             max_treedepth = 10){

  input_data <- data %>%
    dplyr::rename(response = response_col_name) %>%
    dplyr::rename(log_dose = log_dose_col_name) %>%
    dplyr::rename(drug = drug_col_name)

  brms::brm(
    formula = brms::brmsformula(
      response ~ (bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))),
      ec50 + hill + top + bottom ~ 0 + drug, nl = TRUE),
    data = input_data,
    prior = priors,
    inits = inits,
    iter = iter,
    warmup = warmup,
    sample_prior = sample_prior,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
  )
}

