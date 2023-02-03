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
    
## Build the manuscript and vignettes
The manuscript is a multi-part Quarto document in `vignettes_src/manuscript`

Building the manuscript and vignettes requires pandoc, which is available
through Rstudio. But if you are not using Rstudio you please follow the
instructions [here](https://pandoc.org/installing.html).

1) We have used Paperpile to organize the references, to update
```{shell update-references}
make update_references
```

2) To build the manuscript

```{shell build-manuscript}
make manuscript
```

To build the sections into the website

3) Build the website
```{shell build-site}
make build_site
```

To see the site locally, navigate to `BayesPharma/docs/index.html`
    
    
## Package testing and deployment

1) Run all the tests

```{shell test-package}
make test
```




