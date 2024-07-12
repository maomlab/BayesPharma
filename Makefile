
#################################################
# Makefile for building the BayesPharma package #
#################################################


clean:
	rm -rf vignettes_src/manuscript_files
	rm -rf vignettes_src/manuscript/manuscript_cache
	rm -rf vignettes_src/manuscript/manuscript_files/figure-*
	rm -rf vignettes/*.Rmd
	rm -rf vignettes/*.R
	rm -rf vignettes/*.html
	rm -rf vignettes/cache
	rm -rf vignettes/*_files
	rm -rf vignettes/references.bib
	rm -rf vignettes/manuscript.pdf
	rm -rf vignettes/manuscript.docx

clean-vignette-%:
# e.g. make clean-vignette-apply_MuSyC_KCNQ
	rm -rf vignette/${@:clean-vignette-%=%}.Rmd
	rm -rf vignette/${@:clean-vignette-%=%}_files
	rm -rf vignette/cache/${@:clean-vignette-%=%}

deps:
	Rscript -e 'if(!require("devtools")) install.packages("devtools", repos="http://cran.us.r-project.org")'
	Rscript -e "devtools::install_dev_deps()"


build: deps vignettes/references.bib vignettes
	Rscript -e "devtools::document()"
	Rscript -e "devtools::build()"

install:
	Rscript -e "devtools::install_local('.', force = TRUE)"

test:
	Rscript -e "devtools::check()"
	Rscript -e "covr::package_coverage()"
	Rscript -e "lintr::lint_package()"
	Rscript -e "urlchecker::url_check()"
	Rscript -e "docreview::package_review()"
	Rscript -e "spelling::spell_check_package()"

test_alt_builds:
  # this will email the package developer
  Rscript -e "devtools::check_win_devel(quiet = TRUE)"
  Rscript -e "devtools::check_win_release(quiet = TRUE)"
  Rscript -e "devtools::check_win_release(quiet = TRUE)"
  Rscript -e "devtools::check_mac_release(quiet = TRUE)"
  
  # call rhub::validate_email_first(email = <email>) first
  Rscript -e "devtools::check_rhub()"
  Rscript -e "revdepcheck::revdep_check(num_workers = 4)"


#################
# Documentation #
#################

tinytex:
	quarto install tinytex

update_references:
# Set up PaperPile automatic export of bibtex files and generate link
# https://paperpile.com/h/automatic-bibtex-export/
	curl -R -o "vignettes_src/references.bib" -z "vignettes_src/references.bib" https://paperpile.com/eb/qxOGiPoDRL
	rm -f vignettes/references.bib

vignettes/references.bib: vignettes_src/references.bib
	cp vignettes_src/references.bib vignettes/

# compile the vignettes from vignettes_src because they can take quite a while
vignettes/%.Rmd: vignettes_src/%.Rmd vignettes/references.bib
# For each file matching vignettes_src/<vignette>.Rmd call
# cd vignettes && Rscript -e "knitr::knit(input = '../vignettes_src/<vignette>.Rmd', output = '<vignette>.Rmd')"
# This will generate the following files
#    * vignettes/<vignette>.Rmd
#    * vignettes/<vignette>_files/
#        - Defined by `knitr::opts_chunk$set(fig.path = ...)` in the vignette
#    * vignettes/cache/<vignette>/
#        - Defined by `knitr::opts_chunk$set(cache.path = ...)` in the vignette
	cd vignettes &&	Rscript -e "knitr::knit(input = '../$<', output = '$(@F)')"

# check that all the vignettes have been pre-generated
#   vignettes_src/<vignette>.Rmd => vignettes/<vignette>.Rmd
#   pre-computing the each vignette makes building the R-package faster
vignettes: $(patsubst vignettes_src/%,vignettes/%,$(wildcard vignettes_src/*.Rmd))

# quarto renders both the .pdf and .docx version so tell make how to build both
# of these files
vignettes/manuscript.pdf: vignettes/references.bib
	quarto render vignettes_src/manuscript/manuscript.qmd
	mv vignettes_src/manuscript/manuscript.pdf vignettes/
	mv vignettes_src/manuscript/manuscript.docx vignettes/
	
vignettes/manuscript.docx: vignettes/references.bib
	quarto render vignettes_src/manuscript/manuscript.qmd
	mv vignettes_src/manuscript/manuscript.pdf vignettes/
	mv vignettes_src/manuscript/manuscript.docx vignettes/

manuscript: vignettes/manuscript.pdf vignettes/manuscript.docx

site: vignettes manuscript
	quarto render pkgdown/index.qmd
	Rscript -e "pkgdown::build_site()"

all: install vignettes build install

.PHONY: all site manuscript vignettes update_references tinytex test install build deps clean
