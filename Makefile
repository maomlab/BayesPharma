
#################################################
# Makefile for building the BayesPharma package #
#################################################


clean:	
	rm -rf vignettes/*.Rmd
	rm -rf vignettes/*.R
	rm -rf vignettes/*.html
	rm -rf vignettes/cache
	rm -rf vignettes/*_files
	rm -rf vignettes/references.bib

deps:
	Rscript -e "devtools::install_dev_deps()"

build: deps
	Rscript -e "devtools::document()"
	Rscript -e "devtools::build()"

install:
	Rscript -e "devtools::install_local('.', force = TRUE)"

test:
	Rscript -e "devtools::check()"
	Rscript -e "covr::covr()"
	Rscript -e "lintr::lint_package()"

# compile the vignettes from vignettes_src because they can take quite a while

update_references:
# Set up PaperPile automatic export of bibtex files and generate link
# https://paperpile.com/h/automatic-bibtex-export/
	curl -R -o "vignettes_src/references.bib" -z "vignettes_src/references.bib" https://paperpile.com/eb/qxOGiPoDRL
	rm vignettes/references.bib
  
vignettes_src/references.bib:

vignettes/references.bib: vignettes_src/references.bib
	cp vignettes_src/references.bib vignettes/

vignettes/%.Rmd: vignettes_src/%.Rmd vignettes/references.bib
# For each file matching vignettes_src/<vignette>.Rmd call
# cd vignettes && Rscript -e "knitr::knit(input = '../vignettes_src/<vignette>.Rmd', output = '<vignette>.Rmd')"
	cd vignettes &&	Rscript -e "knitr::knit(input = '../$<', output = '$(@F)')"

vignettes: $(patsubst vignettes_src/%,vignettes/%,$(wildcard vignettes_src/*.Rmd))

vignettes/manuscript.qmd: vignettes/references.bib
	quarto render vignettes_src/manuscript/manuscript.qmd --output

manuscript: vignettes_src/manuscript/manuscript.qmd


site: vignettes manuscript
	Rscript -e "pkgdown::build_site()"

all: install vignettes build install

.PHONY: all site manuscript vignettes update_references test install install_no_vignettes build deps clean
