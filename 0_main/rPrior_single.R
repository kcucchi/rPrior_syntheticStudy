
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) < 2) {
  print(length(args))
  print(args[1])
  stop("Two arguments must be provided", call.=FALSE)
}

meas_idx <- args[1]
real_idx <- args[2]

# meas_idx <- 1
# real_idx <- 1

library(rPrior)
library(nimble)

# x vector for densities
vect_x <- seq(from = -15,
              to = 0,
              by = 0.1)

#'
#' first calculate from measurements
#'

# path to rPrior output
pathSave <-
  paste0('../3_rPrior/meas/meas_',meas_idx,'/')
if(!dir.exists(pathSave)){dir.create(pathSave)}

path_res <-
  paste0(pathSave,'rPrior_meas_',meas_idx,
         '_real_',real_idx,'.rds')

if(!file.exists(path_res) | (file.info(path_res)$size == 0)){
  
  if(file.exists(path_res) & file.info(path_res)$size == 0){cat('\nfile',real_idx,'has size 0, redoing the simulations')}
  
  # path to measurements measurements
  path_meas <- 
    paste0('../2_data/meas/meas_',meas_idx,'/',
           'Y_meas_meas_',
           meas_idx,
           '_real_',real_idx,'.rds')
  
  
  # vector of measurements
  Y_meas <- readRDS(file = path_meas)
  
  
  res_gPrior <- 
    rPrior::generalFromMeas(meas = Y_meas,
                            eval_theta = vect_x)
  
  saveRDS(object = res_gPrior,
          file = path_res)
  
}else{
  cat('\nrPrior from measurements already computed!')
}

#'
#' now calculate from moments
#'

pathSave <-
  paste0('../3_rPrior/summary_moments/meas_',meas_idx,'/')
if(!dir.exists(pathSave)){dir.create(pathSave)}

path_res <-
  paste0(pathSave,'rPrior_moments_meas_',meas_idx,
         '_real_',real_idx,'.rds')

if(!file.exists(path_res) | (file.info(path_res)$size == 0)){
  
  if(file.exists(path_res) & file.info(path_res)$size == 0){
    cat('\nfile',real_idx,'for moments has size 0, redoing the simulations')}
  
  # path to moments
  path_moments <- 
    paste0('../2_data/summary_moments/meas_',meas_idx,'/',
           'Y_moments_meas_',
           meas_idx,
           '_real_',real_idx,'.rds')
  
  
  # dataset of moments
  Y_moments <- readRDS(file = path_moments)
  
  
  res_gPrior <- 
    rPrior::regionalized(data = Y_moments,
                         eval_theta = vect_x)
  
  saveRDS(object = res_gPrior,
          file = path_res)
  
}else{
  cat('\nrPrior from moments already computed!')
}


#'
#' finally calculate from bounds
#'

pathSave <-
  paste0('../3_rPrior/summary_bounds/meas_',meas_idx,'/')
if(!dir.exists(pathSave)){dir.create(pathSave)}

path_res <-
  paste0(pathSave,'rPrior_bounds_meas_',meas_idx,
         '_real_',real_idx,'.rds')

if(!file.exists(path_res) | (file.info(path_res)$size == 0)){
  
  if(file.exists(path_res) & file.info(path_res)$size == 0){
    cat('\nfile',real_idx,'for bounds has size 0, redoing the simulations')}
  
  # path to bounds
  path_bounds <- 
    paste0('../2_data/summary_bounds/meas_',meas_idx,'/',
           'Y_bounds_meas_',
           meas_idx,
           '_real_',real_idx,'.rds')
  
  
  # dataset of bounds
  Y_bounds <- readRDS(file = path_bounds)
  
  res_gPrior <- 
    rPrior::regionalized(data = Y_bounds,
                         eval_theta = vect_x)
  
  saveRDS(object = res_gPrior,
          file = path_res)
  
}else{
  cat('\nrPrior from bounds already computed!')
}





