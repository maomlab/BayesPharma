# Bayesian Pharmacology Modeling

Introduction
------------
This package contains a collection of R tools for analyzing pharmacology data using Bayesian statistics and modeling. `BayesPharma` facilitates applying a principled Bayesian workflow to to fit and analyze several foundational pharmacology models, built around the `stan` ecosystem.

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
```{r}
library(tidyverse)
library(BayesPharma)

data <- data.frame(
  response = ...,
  log_dose = ...,
  <predictor columns>)
```
The predictor columns are typically treatment variables like `drug` or batch
variable like `well_id`.

If the treatment dose is given in molar concentration, you can convert it to
`log_dose` using

```{r}
data <- data %>%
  dplyr::mutate(
    log_dose = BayesPharma::calculate_log_dose(dose))
```

The basic usage is

```{r}
model <- BayesPharma::dr_model(
   data = data)
```
### Evaluate model fit

#### Traceplot
```{r}
model %>% BayesPharma::traceplot()
```
#### Basic statistics
```{r}
model %>% basic_stats()
```
#### Regression plot
```{r}
model %>% plot_draws_data()
```
#### Prior densities
```{r}
model %>% prior_densities()
model %>% posterior_densities()
model %>% prior_posterior_densitites()
```
#### posterior predictive check
```{r}
model %>% brms::pp_check(type = "dens_overlay", ndraws = 50)
```

### compare model fits
```{r}
model <- model %>% BayesPharma::add_loo_criterion()
model_fit_comparison <- compare_models(model, model_alt)
```


