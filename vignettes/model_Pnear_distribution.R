## ----set-options, echo=FALSE--------------------------------------------------

# inspired by https://www.jumpingrivers.com/blog/knitr-default-options-settings-hooks/
knitr::opts_chunk$set(
  cache = TRUE,
  echo = FALSE,
  fig.path = "model_Pnear_distribution_figures/knitr-",
  fig.retina = 2, # Control using dpi
  fig.width = 6,  # generated images
  fig.height = 5, # generated images
  fig.pos = "t",  # pdf mode
  fig.align = "center",
  dpi = if (knitr::is_latex_output()) 72 else 300, 
  out.width = "100%")
#  dev = "svg",
#  dev.args = list(png = list(type = "cairo-png")),
#  optipng = "-o1 -quiet")

# cmdstanr is more up-to-date than rstan and runs a little faster
# but can't expose defined functions for downstream analysis
if(Sys.info()["machine"] == "arm64"){
  #rstan does not currently support apple M1 chips
  stan_backend <- "cmdstanr"
} else {
  stan_backend <- "rstan"
}


