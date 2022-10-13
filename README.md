  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/maomlab/BayesPharma/branch/main/graph/badge.svg)](https://app.codecov.io/gh/maomlab/BayesPharma?branch=main)
  <!-- badges: end -->

The [BayesPharma](https://maomlab.github.io/BayesPharma) website package contains a collection of R tools for analyzing pharmacology data using Bayesian statistics.

## Package Development Workflows

### Basic Workflow
The basic workflow follows the package development practices described in the
[R Packages book](https://r-pkgs.org/index.html)


1) Check out the repository using password-protected SSH keys

```{shell clone-repo}
git clone https://github.com/maomlab/BayesPharma.git
cd BayesPharma
```
    
2) Build package the package for the first time

```{r install-package}
# try installing package from github to install dependencies
install.packages("remotes")
remotes::install_github("maomlab/BayesPharma")
    
# build roxygen2 documentation and vignettes
# the vignettes take a while to build so don't built them by default
devtools::document(vignettes = FALSE)

# build the package
devtools::build()
    
# install the package
devtools::install_local(".", force = TRUE)
    
# load the package
library(BayesPharma)
```

3) After editing the package rebuild/reload the package to test it

```{r}
# Run this if you changed function signatures or function documentation
devtools::document(vignettes = FALSE)
    
# this rebuilds the package and loads it 
devtools::load_all()
```    
    
## Package testing and deployment

1) Test the package
```{r}
# This runs the tests in `tests/testthat`
devtools::test()
```

2) Check formatting
```{r}
lintr::lint_package()
```

3) Evaluate test coverage
```{r}
covr::report()
```

4) Full test for CRAN level of compliance
```{r}
devtools::check()
```

5) Build the website
```{r}
pkgdown::build_site()
```
