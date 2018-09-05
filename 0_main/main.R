
library(rPriorSynthetic)

#'
#' # Variables for scripts #
#'

# path to fields
path_fields <- '../1_fields/'

# path to sampled measurements
path_meas <- '../2_data/meas/'
# path to summaries of measurements
path_summary_moments <- '../2_data/summary_moments/'
path_summary_bounds <- '../2_data/summary_bounds/'

# number of realization by measurement configuration
nb_real <- 100

# files param
df_param_meas <- read.csv(file = '2_param_meas.csv',sep=';')

# dummy variable in loop
iLine_meas <- 1

for(iLine_meas in 1:nrow(df_param_meas)){
  
  cat(paste0('\n',iLine_meas,'/',nrow(df_param_meas)))
  
  #'
  #' # Read configuration parameter #
  #'
  
  param_meas <- 
    rPriorSynthetic::csv2vec(pathCsv = '2_param_meas.csv',
                             iLine = iLine_meas)
  
  
  #'
  #' # Generate fields from configurations #
  #'
  
  
  #'
  #' ##  get field idx corresponding to configuration ##
  #'  
  
  # save field setups in dataframe
  iLine_field <- which(
    read.csv(file = '1_param_fields.csv',sep = ';')[,'field_idx'] 
    == 
      param_meas[['field_idx']])
  
  param_field <- 
    rPriorSynthetic::csv2vec(pathCsv = '1_param_fields.csv',
                             iLine=iLine_field)
  
  #'
  #' ##  load fields and generate fields if missing ##
  #' 
  
  # this is the path where the generated fields should be located
  path_field_idx <- 
    paste0(path_fields,'Y_xy_field_',
           as.integer(param_meas[['field_idx']]),'.rds')
  
  if(file.exists(path_field_idx)){
    
    cat('\nloading field')
    
    Y_xy <- 
      readRDS(file = path_field_idx)
    
  }else{
  
    cat('\ngenerating field')
    
    # generate fields
    Y_xy <- 
      rPriorSynthetic::generate_fields(vect_param = param_field,
                                       verbose = T)
    
    saveRDS(object = Y_xy,
            file = path_field_idx)
    
  }
  
  #'
  #' ##  generate measurements if not yet generated ##
  #' 
  
  path_meas_i <- 
    paste0(path_meas,'meas_',
           as.integer(param_meas[['meas_idx']]),'/')
  if(!dir.exists(path_meas_i)){dir.create(path_meas_i)}
  
  for(i_real in 1:nb_real){
    
    path_meas_idx <- 
      paste0(path_meas_i,'Y_meas_meas_',
             param_meas[['meas_idx']],
             '_real_',i_real,'.rds')
    
    if(!file.exists(path_meas_idx)){
      
      cat('\n generate ',i_real,'/',nb_real)
      
      Y_meas <- 
        rPriorSynthetic::generate_meas(vect_meas = param_meas,
                                       Y_xy = Y_xy)
      
      saveRDS(object = Y_meas,
              file = path_meas_idx)
      
    }
    
  }
  
  #'
  #' # Generate summaries if not yet generated #
  #'
  
  path_moments_i <- 
    paste0(path_summary_moments,'meas_',
           as.integer(param_meas[['meas_idx']]),'/')
  path_bounds_i <- 
    paste0(path_summary_bounds,'meas_',
           as.integer(param_meas[['meas_idx']]),'/')
  if(!dir.exists(path_moments_i)){dir.create(path_moments_i)}
  if(!dir.exists(path_bounds_i)){dir.create(path_bounds_i)}
  
  i_real=1
  for(i_real in 1:nb_real){
    
    path_moments_idx <-
      paste0(path_moments_i,'Y_moments_meas_',
             param_meas[['meas_idx']],
             '_real_',i_real,'.rds')
    path_bounds_idx <-
      paste0(path_bounds_i,'Y_bounds_meas_',
             param_meas[['meas_idx']],
             '_real_',i_real,'.rds')
    
    if(!file.exists(path_moments_idx)){
      
      cat('\n generate moments ',i_real,'/',nb_real)
      
      path_meas_idx <- 
        paste0(path_meas_i,'Y_meas_meas_',
               param_meas[['meas_idx']],
               '_real_',i_real,'.rds')
      
      # load vector of measurements
      Y_meas_idx <- readRDS(path_meas_idx)
      
      # calculate moments
      df_moments_idx <-
        as.data.frame(
          dplyr::summarize(.data = dplyr::group_by(.data = Y_meas_idx,site_id),
                           moment.1=mean(val),
                           moment.2=var(val)))
      
      # format for regionalized
      Y_moments_idx <- reshape2::melt(data = df_moments_idx,id="site_id")
      names(Y_moments_idx) <- c('site_id','type','dat')
      
      saveRDS(object = Y_moments_idx,
              file = path_moments_idx)
      
    }
    
    if(!file.exists(path_bounds_idx)){
      
      cat('\n generate bounds ',i_real,'/',nb_real)
      
      path_meas_idx <- 
        paste0(path_meas_i,'Y_meas_meas_',
               param_meas[['meas_idx']],
               '_real_',i_real,'.rds')
      
      # load vector of measurements
      Y_meas_idx <- readRDS(path_meas_idx)
      
      
      # calculate bounds
      df_bounds_idx <-
        as.data.frame(
          dplyr::summarize(.data = dplyr::group_by(.data = Y_meas_idx,site_id),
                           bound.min=min(val),
                           bound.max=max(val)))
      
      # format for regionalized
      Y_bounds_idx <- reshape2::melt(data = df_bounds_idx,id="site_id")
      names(Y_bounds_idx) <- c('site_id','type','dat')
      
      saveRDS(object = Y_bounds_idx,
              file = path_bounds_idx)
      
    }
    
  }
  
  
  #'
  #' ##  Call rPrior using commands ##
  #' 
  
  for(i_real in 1:nb_real){
    
    cat(paste0('\n ---- ',i_real,'/',nb_real,' ----'))
    
    system(paste0('RScript --vanilla --default-packages=methods,utils,stats rPrior_single.R ',
                  param_meas[['meas_idx']] ,'   ',i_real))
    
    
  }
  
}



