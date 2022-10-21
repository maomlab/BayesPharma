
clean:
	rm -rf vignettes/*.Rmd
	rm -rf vignettes/cache
	rm -rf vignettes/*_files

build:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::build()"

test:
	Rscript -e "devtools::test()"
	Rscript -e "covr::covr()"
	Rscript -e "lintr::lint_package()"

# compile the vignettes from vignettes_src because they can take quite a while
vignettes/%.Rmd: vignettes_src/%.Rmd
	cd vignettes &&	Rscript -e "knitr::knit(input = '../$<', output = '$(@F)')"

all_vignettes: $(patsubst vignettes_src/%,vignettes/%,$(wildcard vignettes_src/*.Rmd))

vignettes/manuscript.qmd:
	quarto render vignettes_src/manuscript/manuscript.qmd --output-dir vignettes

build_site: all_vignettes vignettes/manuscript.qmd
	Rscript -e "pkgdown::build_site()"

install:
	Rscript -e "devtools::install_local('.', force = TRUE)"

