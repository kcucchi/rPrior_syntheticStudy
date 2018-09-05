
library(rPriorSynthetic)


#'
#'  load synthetic field sites
#'  

Y_xy <- readRDS(file = '../1_fields/Y_xy_field_1.rds')

#'
#' # Define how many measurements we want to sample #
#'

# eventually, but start simple
vect_J_1 <- c(2,5,10,20,30)
vect_J_2 <- c(5,20)

if(file.exists('df_res_sec3.1.2.rds')){
  df_res <- readRDS(file = 'df_res_sec3.1.2.rds')
}else{
  df_res <- 
    data.frame(expand.grid(J_1=vect_J_1,
                           J_2=vect_J_2,
                           real_idx=1:100,
                           S_idx1=NA,
                           S_idx2=NA,
                           KLD_1=NA,
                           KLD_2=NA,
                           KLD_12=NA,
                           KLD_same=NA,
                           a=NA,
                           b=NA))
  df_res$simu_idx <- 1:nrow(df_res)
  
  head(df_res)
  
  df_res$S_idx1 <- sample(x = 1:100,size = nrow(df_res),replace = T)
  df_res$S_idx2 <- sample(x = 1:100,size = nrow(df_res),replace = T)
  
  # some simulations have twice the same site
  which(df_res$S_idx1 == df_res$S_idx2)
  # in that case, add 1 to the site number
  df_res[which(df_res$S_idx1 == df_res$S_idx2),'S_idx2'] <- 
    df_res[which(df_res$S_idx1 == df_res$S_idx2),'S_idx2'] + 1
  # and if that results in site 101 then assign 1
  df_res[which(df_res$S_idx2 == 101),'S_idx2'] <- 1
  
  # add "S_" at start of name
  df_res$S_idx1 <- paste0('S',df_res$S_idx1)
  df_res$S_idx2 <- paste0('S',df_res$S_idx2)
  
  head(df_res)
  
}



# set.seed(1)
# 
# i_row <- 1
# 
# # randomly select 2 field sites
# site_names <- paste0('S',sample(x = 1:100,size = 2))
# df_res[i_row,c('S_idx1','S_idx2')] <- c('S1','S2')
# df_res[i_row,]

#'
#' sample measurements from sites 
#'

# I can't use rPriorSynthetic::generate_meas 
# because it only allows for same number of measurements per site
# so, implement it here

if(file.exists('Y_meas.rds')){
  Y_meas <- readRDS(file = 'Y_meas.rds')
}else{
  
  # this will contain measurement values at all sites
  Y_meas <-
    data.frame(simu_idx=numeric(0),
               id_meas=numeric(0),
               site_id=numeric(0),
               val=numeric(0),
               x=numeric(0),
               y=numeric(0))
  
  for(i_row in 1:nrow(df_res)){ # loop over all realizations for results
    
    cat('\n',i_row,'/',nrow(df_res))
    
    # define parameters for measurement sampling
    vect_meas <- c(I=2,
                   J_1=df_res[i_row,'J_1'],
                   J_2=df_res[i_row,'J_2'],
                   r=10)
    
    vect_meas
    
    for(i_site in 1:2){ # only 2 sites here
      
      ### generate measurement locations
      
      res <- diff(sort(unique(Y_xy$x))[1:2])
      size <- max(unique(Y_xy$x))
      
      ## uniform sampling within circle
      # sample distance from center
      rho <- sqrt(runif(vect_meas[[paste0('J_',i_site)]]))
      # sample angles
      theta <- runif(vect_meas[[paste0('J_',i_site)]], 0, 2*pi)
      # transform to x,y at res solution
      Y_meas_i_site <- data.frame(
        x = res *
          round( (size / 2 +
                    vect_meas[['r']] * rho * cos(theta)) / res ),
        y = res *
          round( (size / 2 +
                    vect_meas[['r']] * rho * sin(theta)) / res ),
        id_meas = 1:vect_meas[[paste0('J_',i_site)]],
        site_id = as.character(df_res[i_row,c('S_idx1','S_idx2')][i_site]),
        simu_idx = df_res[i_row,'simu_idx']
      )
      
      # merge measurement values at x and y
      Y_meas_i_site <-
        dplyr::left_join(
          x = Y_meas_i_site,
          y = Y_xy[,c("x","y",as.character(df_res[i_row,c('S_idx1','S_idx2')][i_site]))],
          by = c("x","y"))
      
      # change name of value field
      names(Y_meas_i_site)[which(names(Y_meas_i_site) == 
                                   df_res[i_row,paste0('S_idx',i_site)])] <- 'val'
      
      # shuffle columns
      Y_meas_i_site <- Y_meas_i_site[,names(Y_meas)]
      
      # append to main dataframe
      Y_meas <- rbind(Y_meas,Y_meas_i_site)
      
      rm(Y_meas_i_site)
      
    } # end loop for sites
    
    
  }
  
  
  # format Y_meas for regionalized
  names(Y_meas)[which(names(Y_meas) == "val")] <- "dat"
  Y_meas$type <- 'meas'
  
  head(Y_meas)
  
  saveRDS(object = Y_meas,file = 'Y_meas.rds')
  
}


#'
#' calculate rPrior 
#'



# i_row <- 1

head(which(is.na(df_res$KLD_1)))

for(i_row in which(is.na(df_res$KLD_1))){
  
  # for tracking simulations  
  line=paste0(i_row,'/',nrow(df_res),
              '\t',round(i_row/nrow(df_res)*100),'%',
              '\t',Sys.time())
  cat(line)
  write(line,file="log.txt",append=TRUE)
  
  # define in subfile
  system(paste0(
    'RScript --vanilla --default-packages=methods,utils,stats sub_diff_measPerSite.R ',
    i_row))
  
}
