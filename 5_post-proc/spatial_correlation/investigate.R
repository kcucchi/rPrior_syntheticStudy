
#' ---
#' title : Investigate results with and without MVN for spatial autocorrelation
#' author : Karina Cucchi
#' date : January 2018
#' ---

library(ggplot2)

#'
#' # Define setup of simulations #
#'

# define simulations
df_simu <- 
  data.frame(meas_idx=rep(x = c(91,78,65),times = 2),
             real_idx=rep(x = c(15,31,1), times = 2),
             type = rep(x = c('meas','meas_spatial'),each = 3))
df_simu$simu_idx = 1:nrow(df_simu)

#'
#' # Load corresponding data #
#'


#'
#' ## Read rPrior results ##
#' 

# load in list
list_rPrior <- list()

for(i_simu in 1:nrow(df_simu)){
  
  # read one without spatial component
  path_rPrior <- 
    paste0('../../3_rPrior/',df_simu[i_simu,'type'],
           '/meas_',df_simu[i_simu,'meas_idx'],
           '/rPrior_meas_',df_simu[i_simu,'meas_idx'],
           '_real_',df_simu[i_simu,'real_idx'],'.rds')
  list_rPrior[[i_simu]] <- readRDS(path_rPrior)
  
}
rm(i_simu)


#'
#' ##  read underlying density ##
#'  

dens_Y_all <- 
  readRDS(file = '../../4_calc_res/dens_Y_all.rds')
names(dens_Y_all) <- c("x","y_underlying")


#'
#' ## Get corresponding calculated KLD ##
#'

df_results_kld <-
  readRDS(file = '../../4_calc_res/df_results_kld.rds')

df_simu$type <- as.character.factor(df_simu$type)

df_simu <- 
  dplyr::left_join(x = df_simu,
                   y = df_results_kld,
                   by = c("meas_idx","real_idx","type"))


#'
#' ## join all with parameters of simulations ##
#'  

df_setup <-
  read.csv(file = '../../0_main/2_param_meas.csv',
           sep=';')

# add parameters of simulation for each meas_idx/real_idx combination
df_simu <- 
  dplyr::left_join(
    x = df_simu,
    y = df_setup,
    by="meas_idx")

print(df_simu)

# ok, all data is loaded now

#'
#' # Compare effective sizes of MCMC chains #
#' 

for(idx_simu in 1:nrow(df_simu)){
  
  cat('\n')
  cat(paste(names(df_simu)))
  cat('\n')
  cat(paste(df_simu[idx_simu,]))
  cat(' --- ')
  cat(list_rPrior[[idx_simu]]$MCMC$MCMC_effectiveSizes)
  cat (' --- ')
  cat(100 * list_rPrior[[idx_simu]]$MCMC$MCMC_effectiveSizes / 
        nrow(list_rPrior[[idx_simu]]$MCMC$MCMCsamples))
  cat('\n')
}

#' 
#' # Plot all results #
#' 

#'
#' ## prepare dataframe for plotting ##
#'

# store rPrior results in single dataframe
df_rPrior <- 
  data.frame(x=list_rPrior[[1]]$rPrior$x)

for(i_simu in 1:nrow(df_simu)){
  df_rPrior[,paste0('y_simu_',i_simu)] <- list_rPrior[[i_simu]]$rPrior$y
}

# df_rPrior <-
#   dplyr::left_join(x = df_rPrior,y = dens_Y_all,by = 'x')
# head(df_rPrior)

df_forPlotting <-
  reshape2::melt(df_rPrior,id = 'x')

#'
#'  add indicator for simulation index
#'  

df_forPlotting$simu_idx <- 
  as.integer(unlist(lapply(X = strsplit(as.character.factor(df_forPlotting$variable),split='_'),
                           FUN = function(x) x[[3]])))
head(df_forPlotting)

#'
#' add parameters of simulation
#'

df_forPlotting <- 
  dplyr::left_join(x = df_forPlotting,
                   y = df_simu,
                   by = "simu_idx")

head(df_forPlotting)

#'
#' ## now plot ##
#'

g_rPrior_comparison <-
  ggplot() +
  geom_line(data=dens_Y_all,mapping = aes(x=x,y=y_underlying),linetype='dotted') +
  geom_line(data = df_forPlotting,
            mapping = aes(x=x,y=value,
                          col=factor(r),linetype=type)) +
  labs(x = 'y',y = expression('f'['Y']*'(y)'),
       col = expression('r/'*lambda),linetype = "spatial\n autocorrelation") +
  scale_linetype_manual(values=c('meas'='solid','meas_spatial'='dashed'),
                        labels = c("meas"="not accounting","meas_spatial"="accounting")) +
  theme_bw() +
  facet_wrap( ~ r) +
  theme(legend.position = 'bottom')

g_rPrior_comparison

pdf('g_rPrior_comparison.pdf',width = 7,height = 3)
print(g_rPrior_comparison)
dev.off()


#'
#' # Investigate distributions of hyperparameters #
#'

# goal is to plot the distribution of hyperparameters for each case
# in order to compare results between MVNs and site-specific independence
# in the plot, put values of r horizontally and hyperparameters vertically


#'
#' ## Save densities in dataframe ##
#'

# create empty dataframe
df_results_hyperPar <- 
  data.frame(simu_idx = numeric(0),
             hyperPar = character(0),
             x = numeric(0),
             y = numeric(0))

# loop over all simulations
for(i_simu in 1:nrow(df_simu)){
  
  # read corresponding rPrior result
  rPrior_iSimu <- list_rPrior[[i_simu]]
  
  # this is the vector containing the names of hyperparameters
  vect_hyperParam <- rPrior_iSimu$hyperPar
  
  #  loop over hyperparameters
  for(i_hyper in 1:length(vect_hyperParam)){
    df_results_hyperPar <- 
      rbind(df_results_hyperPar,
            data.frame(simu_idx = i_simu,
                       hyperPar = vect_hyperParam[i_hyper],
                       x = rPrior_iSimu$d_hyperPar$d_hyperPar_post[[i_hyper]]$x,
                       y = rPrior_iSimu$d_hyperPar$d_hyperPar_post[[i_hyper]]$y))  
  }
  
}


#'
#' join with parameters of simulations
#' 

df_results_hyperPar <- 
  dplyr::left_join(x = df_results_hyperPar,
                   y = df_simu,
                   by = "simu_idx")

head(df_results_hyperPar)
unique(df_results_hyperPar$hyperPar)

#'
#' ## read underlying true values of hyperparameters ##
#'

# read values of hyperparameters
df_param_fields <-
  read.csv(file = '../../0_main/1_param_fields.csv',
           sep=';')

df_param_fields$sigma <- sqrt(df_param_fields$sigma_2)
df_param_fields$tau <- sqrt(df_param_fields$tau_2)

# names(df_param_fields)[2:ncol(df_param_fields)] <- 
#   paste0(names(df_param_fields)[2:ncol(df_param_fields)],'_0')

# reshape to have a column for hyperparameter name and a column for hyperparameter value
df_param_fields_long <- 
  reshape2::melt(df_param_fields,id.vars = 'field_idx',
                 variable.name='hyperPar',value.name="x_0")

# coerce hyperPar to character (to avoid problem with lambda when joining)
df_param_fields_long$hyperPar <- 
  as.character.factor(df_param_fields_long$hyperPar)
df_results_hyperPar$hyperPar <- 
  as.character.factor(df_results_hyperPar$hyperPar)

df_results_hyperPar <- 
  dplyr::left_join(x = df_results_hyperPar,
                   y = df_param_fields_long,
                   by = c('field_idx','hyperPar'))
head(df_results_hyperPar)

#'
#' ## finally make the plot ##
#'

g_hyperPar_comparison <-
  ggplot(data = subset(df_results_hyperPar,hyperPar != 'lambda')) +
  geom_line(mapping = aes(x=x,y=y,
                          col=factor(r),linetype=type)) +
  geom_vline(mapping = aes(xintercept = x_0)) +
  labs(x = 'y',y = expression('f'['Y']*'(y)'),
       col = expression('r/'*lambda),linetype = "spatial\n autocorrelation") +
  scale_linetype_manual(values=c('meas'='solid','meas_spatial'='dashed'),
                        labels = c("meas"="not accounting","meas_spatial"="accounting")) +
  theme_bw() +
  facet_wrap(hyperPar ~ r,scales = 'free') +
  theme(legend.position = 'bottom')

print(g_hyperPar_comparison)

pdf(file = 'g_hyperPar_comparison.pdf',width = 7,height = 6)
print(g_hyperPar_comparison)
dev.off()


#'
#' ## Plot hyperdistribution of lambda ##
#'

g_lambda_comparison <-
  ggplot(data = subset(df_results_hyperPar,hyperPar == 'lambda')) +
  geom_line(mapping = aes(x=x,y=y,
                          col=factor(r),linetype=type)) +
  geom_vline(mapping = aes(xintercept = x_0)) +
  labs(x = 'y',y = expression('f'['Y']*'(y)'),
       col = expression('r/'*lambda),linetype = "spatial\n autocorrelation") +
  scale_linetype_manual(values=c('meas'='solid','meas_spatial'='dashed'),
                        labels = c("meas"="not accounting","meas_spatial"="accounting")) +
  theme_bw() +
  facet_wrap(hyperPar ~ r,scales = 'free') +
  theme(legend.position = 'bottom')

print(g_lambda_comparison)

pdf(file = 'g_lambda_comparison.pdf',width = 7,height = 3)
g_lambda_comparison
dev.off()


