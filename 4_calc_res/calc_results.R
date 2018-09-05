
library(ggplot2)

#'
#' path to rPrior folder
#'

# local
# path2rPrior <- '../3_rPrior/'
# when on NERSC
path2rPrior <- '/global/cscratch1/sd/kcucchi/project_priors/3_rPrior/'

#'
#' # first step #
#'

# read setup file

df_setup <-
  read.csv(file = '../0_main/2_param_meas.csv',
           sep=';')

#'
#' # Calculate underlying density at all sites #
#'

# for now just one underlying synthetic truth, 
# so just calculate the density at this site

fields_idx = 1
Y_xy <- readRDS(file = paste0('../1_fields/Y_xy_field_',fields_idx,'.rds'))

# x vector for densities
vect_x <- seq(from = -15,
              to = 0,
              by = 0.1)

# this is the densities of all values at the sites
Y_all <- as.vector(as.matrix(Y_xy[,grep(pattern = '^S',x = names(Y_xy))]))
dens_Y_all <- density(Y_all)
# interpolate on x reference vector
# for comparison with gPrior results
dens_Y_all <- 
  as.data.frame(
    approx(x = dens_Y_all$x,y = dens_Y_all$y,xout = vect_x,
           yleft=0,yright = 0))

rm(Y_xy,Y_all,vect_x,fields_idx)

# save in rds file
saveRDS(object = dens_Y_all,file = 'dens_Y_all.rds')

#'
#' # Calculate underlying density for mu at all sites #
#'

df_fields <-
  read.csv(file = '../0_main/1_param_fields.csv',
           sep=';')

df_fields

# the underlying assumption for the mean is 
# normal with mean -7.5 and variance 0.25
dens_mu_all <- 
  data.frame(x = dens_Y_all$x,
             y = dnorm(x = dens_Y_all$x,mean = -7.5,sd = sqrt(0.25)))

# ggplot(dens_mu_all) + geom_line(mapping = aes(x,y))

#'
#' # Loop over all results and calculate KLD between underlying and predicted #
#'
#'
#' calculate both for Y and mu


nb_real <- 100

# x vector for densities
vect_x <- seq(from = -15,
              to = 0,
              by = 0.1)

# initialize results dataframe
# contains KLD for both Y and mu distributions
df_results <- 
  data.frame(expand.grid(meas_idx=1:nrow(df_setup),
                         real_idx=1:nb_real,
                         type=c('meas','meas_spatial','moments','bounds'),
                         variable=c('Y','mu'),
                         stringsAsFactors = F),
             stringsAsFactors = F)
df_results$kld <- NA

# if exists, add previously computed kld values
if(file.exists('df_results_kld.rds')){
  
  df_results_temp <- readRDS('df_results_kld.rds')
  
  df_results <- 
    dplyr::left_join(x=df_results,
                     y=df_results_temp[,c("meas_idx","real_idx","type","kld")],
                     by=c("meas_idx","real_idx","type","variable"))
}

# these are row indices corresponding to missing values
vect_missing <- which(is.na(df_results$kld))

i_row <- 
  which(df_results$meas_idx==1 & 
          df_results$real_idx==1 & 
          df_results$type == "moments")

# fill in results array
if(length(vect_missing)>0){
  
  for(idx_missing in 1:length(vect_missing)){
    
    cat('\n',idx_missing,'/',length(vect_missing))
    
    i_row <- vect_missing[idx_missing]
    
    # define path to rPrior (depends on type - meas, meas_spatial, bounds or moments)
    if(df_results[i_row,'type']=="meas"){
      path2rPrior_idx <-
        paste0(path2rPrior,'meas/meas_',df_results[i_row,'meas_idx'],
               '/rPrior_meas_',df_results[i_row,'meas_idx'],
               '_real_',
               df_results[i_row,'real_idx'],'.rds')
    }else if(df_results[i_row,'type']=="meas_spatial"){
      path2rPrior_idx <-
        paste0(path2rPrior,'meas_spatial/meas_',df_results[i_row,'meas_idx'],
               '/rPrior_meas_',df_results[i_row,'meas_idx'],
               '_real_',
               df_results[i_row,'real_idx'],'.rds')
    }else if(df_results[i_row,'type']=="bounds"){
      path2rPrior_idx <-
        paste0(path2rPrior,'summary_bounds/meas_',df_results[i_row,'meas_idx'],
               '/rPrior_bounds_meas_',df_results[i_row,'meas_idx'],
               '_real_',
               df_results[i_row,'real_idx'],'.rds')
    }else if(df_results[i_row,'type']=="moments"){
      path2rPrior_idx <-
        paste0(path2rPrior,'summary_moments/meas_',df_results[i_row,'meas_idx'],
               '/rPrior_moments_meas_',df_results[i_row,'meas_idx'],
               '_real_',
               df_results[i_row,'real_idx'],'.rds')
    }
    
    cat('\n',path2rPrior_idx)
    
    if(file.exists(path2rPrior_idx) & 
       file.info(path2rPrior_idx)$size > (2000 * 1e3)){
      
      rPrior_idx <- readRDS(file = path2rPrior_idx)

      # if i_row corresponds to Y, calculate kld for Y distributions
      if(df_results[i_row,'variable'] == "Y"){
        df_results[i_row,'kld']  <-
          rPrior::KL_divergence(
            theta = vect_x,
            # p is the reference
            p_theta = rPrior_idx$rPrior$y, 
            q_theta = dens_Y_all$y)      
        # so that we have high kld value when 
        # readRDS(file = path2rPrior)$rPrior$y is high (numerator) and
        # dens_Y_all$y is small (denominator)  
        
      }else if(df_results[i_row,'variable'] == "mu"){
        
        ## shit, need to calculate the posterior distribution for mu
        ## from MCMC samples of alpha and tau
        # need to sample from N(alpha,tau)
        samples_alpha <- rPrior_idx$MCMC$MCMCsamples[,'alpha']
        samples_tau <- rPrior_idx$MCMC$MCMCsamples[,'tau']
        samples_mu <- rnorm(n = 1000,mean = samples_alpha,sd = abs(samples_tau))
        d_mu <- density(samples_mu,from = min(dens_mu_all$x),to=max(dens_mu_all$x))
        d_mu <- rPrior::normalize_pdf(d_mu$x,d_mu$y)
        # interpolate on same values than dens_mu_all
        p_rPrior_mu_idx_y <- approx(x = d_mu$x,
                                    y = d_mu$p_x,
                                    xout = dens_mu_all$x,
                                    yleft=0,yright=0)$y
        
        
        # finally calculate the divergence
        df_results[i_row,'kld'] <-
          rPrior::KL_divergence(
            theta = dens_mu_all$x,
            # p is the reference
            p_theta = p_rPrior_mu_idx_y, 
            q_theta = dens_mu_all$y) 
      }
      
    }else{cat(' raf')}
    
  }
  
}


#'
#' ## save in rds file ##
#'

# write in table
saveRDS(object = df_results,file = 'df_results_kld.rds')

