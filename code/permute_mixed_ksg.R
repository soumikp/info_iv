library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  reticulate, 
  #--- Packages to test code
  microbenchmark, 
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra,
  #--- Packages to Produce Figures
  ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, 
  gridExtra, survminer, viridis, ggridges, hrbrthemes,
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr
)

source_python(file.path(here(), "code", "mixed.py"))

permute_test <- function(x, y, nperm = 5000){
  obs_mi <- abs(Mixed_KSG(x, y))
  permute_mi <- replicate(nperm, 
                          abs(Mixed_KSG(x, y[sample(nrow(y)), ])))
  return (sum(permute_mi > obs_mi)/length(permute_mi))
}



