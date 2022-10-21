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
    
2) Build the package

```{shell install-package}
make clean
make deps
make build
make install
```


4) The edit and re-load from during development 

```{r edit-and-run}
devtools::document()
devtools::load_all()
```    
    
## Package testing and deployment

1) Run all the tests

```{shell test-package}
make test
```

5) Build the website
```{shell build-site}
make build_site
```

To see the site locally, navigate to `BayesPharma/docs/index.html`


