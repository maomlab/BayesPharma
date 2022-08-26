## ----set-options, echo=FALSE--------------------------------------------------
knitr::opts_chunk$set(
  cache = TRUE,
  echo = FALSE,
  fig.path = "model_Enzyme_Kinetics/knitr-",
  fig.retina = 2, # Control using dpi
  fig.width = 6,  # generated images
  fig.height = 5, # generated images
  fig.pos = "t",  # pdf mode
  fig.align = "center",
  dpi = if (knitr::is_latex_output()) 72 else 300, 
  out.width = "100%")

output_path <- "/tmp/model_stan_tQ_multiple"
if(!dir.exists(output_path)){
  dir.create(output_path)
}

