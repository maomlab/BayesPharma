#'
#'
#'
#'
#'

dose_response_formula <- function(predictors = 1, ...){

  predictor_formula = rlang::new_formula(lhs = quote(ec50 + hill + top + bottom),
                                         rhs = rlang::enexpr(predictors))

  sigmoid_formula = brms::brmsformula(
    response ~ (bottom + (top - bottom) / (1 + 10^((ec50 - log_dose)*hill))),
    predictor_formula, nl = TRUE, ...)

  return(sigmoid_formula)
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

dose_response_model <- function(data,
                                response_col_name,
                                log_dose_col_name,
                                sigmoid_formula = dose_response_formula(),
                                priors = NULL,
                                inits = 0,
                                iter = 8000,
                                control = list(adapt_delta = 0.99),
                                ...){

  if (is.null(priors)){
    stop("priors for ec50, hill, top and bottom are required. Use make_priors function to get default priors.")
  }

  # this will probably be removed later
  # will say the input tibble/data.frame needs to contain response and log_dose
  input_data <- data %>%
    dplyr::rename(response = response_col_name) %>%
    dplyr::rename(log_dose = log_dose_col_name)

  brms::brm(
    formula = sigmoid_formula,
    data = input_data,
    prior = priors,
    inits = inits,
    control = control,
    ...)
  }
