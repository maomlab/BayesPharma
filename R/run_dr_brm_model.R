





dr_bayes_model <- function(data,
                        priors = NULL,
                        inits = 0,
                        iter = 8000,
                        warmup = 4000,
                        chains = 4,
                        adapt_delta = 0.99,
                        max_treedepth = 10
                        ){
  brms::brm(
    formula = brms::brmsformula(
      response ~ (bottom + (top - bottom) / (1 + 10^((ic50 - log_dose)*hill))),
      ic50 + hill + top + bottom ~ 1, nl = TRUE),
    data = data,
    prior = priors,
    inits = inits,
    iter = 8000,
    warmup = 4000,
    control = list(adapt_delta = 0.99, max_treedepth = 10)
  )
  }
