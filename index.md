# Bayesian Pharmacology Modeling

Introduction
------------
The `BayesPharma` package contains a collection of R tools for analyzing pharmacology data using Bayesian statistics and modeling. `BayesPharma` relies on the `stan` ecosystem and `brms` package. `BayesPharma` facilitates applying a principled Bayesian workflow to to fit and analyze several foundational pharmacology models, such as dose-response modeling, modeling pnear and folding funnels from molecular modeling, and modeling potential docking 'hit-rate' curves as a function of dock score from ultra-large library docking ([Lyu et al. (2019)](https://www.nature.com/articles/s41586-019-0917-9), [Alon et al. (2021)](https://www.nature.com/articles/s41586-021-04175-x)). 

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
data <- data |> log_dose = BayesPharma::calculate_log_dose(dose)
```

The basic usage is

```{r}
model <- BayesPharma::dr_model(
   data = data)
```
### Evaluate model fit

#### Traceplot
```{r}
model |> BayesPharma::traceplot()
```
#### Basic statistics
```{r}
model |> BayesPharma::basic_stats()
```
#### Regression plot
```{r}
model |> BayesPharma::posterior_draws_plot()
```
#### Prior densities
```{r}
model |> BayesPharma::density_distributions()
model |> BayesPharma::posterior_densities()
model |> BayesPharma::prior_posterior_densitites()
```
#### posterior predictive check
```{r}
model |> brms::pp_check(type = "dens_overlay", ndraws = 50)
```

### compare model fits
```{r}
model <- model |> brms::add_loo_criterion()
model_fit_comparison <- brms::compare_models(model, model_alt)
```


