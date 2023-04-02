library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
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

source(file.path(here(), "code", "permute_mixed_ksg.R"))


data_sim_pat1 <- function(n, a){
  x <- rnorm(n)
  z <- rnorm(n)
  
  e <- MASS::mvrnorm(n, 
                     mu = c(0, 0), 
                     Sigma = cbind(c(1, 0.8), c(0.8, 1)))
  
  t <- x + a*(z + z*z) + e[,1]
  y <- x + t + e[,2]
  
  return(cbind(z = z, t = t, y = y, z = x))
}

data <- data_sim_pat1(500, 0)

#relevance 
nperm = 50
obs_mi <- abs(Mixed_KSG(as.matrix(data[,1], ncol = 1), 
                        as.matrix(data[,2], ncol = 1)))
permute_mi <- replicate(nperm,
                        abs(Mixed_KSG(as.matrix(data[,1], ncol = 1), 
                                      as.matrix(data[sample(nrow(data)),2], ncol = 1))))
pval <- (sum(permute_mi > obs_mi)/length(permute_mi))

#exclusion
gam()
