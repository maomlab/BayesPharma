# Bayesian Pharmacology Modeling

Introduction
------------
This package contains a collection of R tools for analyzing pharmacology data using Bayesian statistics.


Installation
------------

### Pre-requisites
Follow the instructions to install [rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)

### Install `BayesPharma`
In R do
```{r}
   install.packages("remotes")
   remotes::install_github("maomlab/BayesPharma", build_vignettes = FALSE)
```

Usage
-----
calculate_log_dose 
change_col_name


library(tidyverse)
library(BayesPharma)

data <- data.frame(
  response = ...,
  log_dose = ...,
  <predictor columns>)


dr_inits()
dr_priors()

model <- data %>%
  dr_model(
    formula = dr_formula

model_prior <- 

### Evaluate model fit

#### Traceplot
model %>% BayesPharma::traceplot()

#### Basic statistics
model %>% basic_statistics()

#### Regression plot
model %>% plot_draws_data()

#### Prior densities
model %>% prior_densities()
model %>% posterior_densities()
model %>% prior_posterior_densitites()

#### posterior predictive check
model %>% brms::pp_check(type = "dens_overlay", ndraws = 50)



### compare model fits
model <- model %>% BayesPharma::add_loo_criterion()
model_fit_comparison <- compare_models(model, model_alt)




#### TODO
either remove or re-organize column renaming functions

dr_formula
remove multiple_perturbations, just handle that case with
dr_formula <- function (
  predictors = 1,
  ...) {
  brms::brmsformula(
    response ~ sigmoid(ec50, hill, top, bottom, log_dose),
    rlang::new_formula(
      lhs = quote(ec50 + hill + top + bottom),
      rhs = rlang::enexpr(predictors)),
    nl = TRUE,
    ...)
}

  

Add name to model object
Use model name in add_loo_criterion
rename dr_inits to have them better go with the model
in dr_inits check data types
do we need to have nchains for dr_inits?
plot trajectories
   put required arguments before optional arguments
   change measurement to response and make it handle strings or expressions
remove plot_pp_check


