#'
#'---
#'title: plot reg priors for site-specific parameters
#'author: Karina Cucchi
#'date: March 4th 2018
#'---
#'

library(ggplot2)

library(dplyr)

#'
#' # Load data #
#'

# to store graphics
list_plots <- list()

vect_idx <- seq(from=0,to=95,by=5)

idx=0

pdf(file = 'g_site_specific_all.pdf',width = 5,height = 5)

for(i in 1:length(vect_idx)){
  
  idx <- vect_idx[i]
  
  print(idx)
  
  
  #'
  #' ## Define setup of simulations ##
  #'
  
  
  nb_config = 5
  nb_plotByConfig=5
  
  # define simulations
  df_simu <- 
    data.frame(meas_idx=rep(x = round(seq(from=54,to=65,length.out = nb_config)),
                            each = nb_plotByConfig),
               real_idx=rep(x = idx+(1:5), times = nb_config),
               type = rep(x = c('meas_spatial'),each = nb_config*nb_plotByConfig))
  df_simu$simu_idx = 1:nrow(df_simu)
  
  head(df_simu)
  
  #'
  #' ## Join with parameters of simulation ##
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
  
  # print(df_simu)
  
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
  #' # Plot reg priors of site-specific parameter #
  #'
  
  df_plot <- 
    data.frame(simu_idx=numeric(),par=character(),
               x=numeric(),y=numeric())
  
  for(i in 1:nrow(df_simu)){
    
    rPrior_i <- list_rPrior[[i]]
    
    # start by saving y values
    df_plot <- 
      rbind(
        df_plot,
        data.frame(
          simu_idx=i,
          par='y',
          x=rPrior_i$rPrior$x,
          y=rPrior_i$rPrior$y))
    
    df_plot <- 
      rbind(
        df_plot,
        data.frame(
          simu_idx=i,
          par='lambda',
          x=rPrior_i$d_hyperPar$d_hyperPar_post[[which(rPrior_i$hyperPar == 'lambda')]]$x,
          y=rPrior_i$d_hyperPar$d_hyperPar_post[[which(rPrior_i$hyperPar == 'lambda')]]$y))
    
    
    df_plot <- 
      rbind(df_plot,
            data.frame(
              simu_idx=i,
              par='sigma',
              x=rPrior_i$d_hyperPar$d_hyperPar_post[[which(rPrior_i$hyperPar == 'sigma')]]$x,
              y=rPrior_i$d_hyperPar$d_hyperPar_post[[which(rPrior_i$hyperPar == 'sigma')]]$y))
    
    # estimate density of mu from densities of alpha and tau
    MCMC_alpha_i <- rPrior_i$MCMC$MCMCsamples[,"alpha"]
    MCMC_tau_i <- rPrior_i$MCMC$MCMCsamples[,"tau"]
    MCMC_mu_i <- rnorm(n = length(MCMC_alpha_i),mean = MCMC_alpha_i,sd = MCMC_tau_i)
    dens_mu_i <- density(MCMC_mu_i,from = -15,to=0)
    
    df_plot <- 
      rbind(df_plot,
            data.frame(
              simu_idx=i,
              par='mu',
              x=dens_mu_i$x,
              y=dens_mu_i$y))
    
  }
  
  #'
  #' ## Join with parameters of simulations ##
  #'
  
  df_plot <-
    dplyr::left_join(x = df_plot,
                     y = df_simu,
                     by = "simu_idx")
  
  # select only I=3,13,30
  df_plot <-
    df_plot %>%
    subset(I %in% c(3,13,30))
  
  df_plot$par <- factor(x = df_plot$par,levels=c("mu","sigma","lambda"))
  
  
  # this is the underlying density for mu
  MCMC_mu_0 <- rnorm(n = 100000,mean = -7.5,sd = 0.5)
  dens_mu_0 <- density(MCMC_mu_0,from = -15,to=0)
  df_mu_0 <- data.frame(x=dens_mu_0$x,y=dens_mu_0$y)
  
  # plot for mu
  g_mu <- 
    ggplot() +
    geom_line(data = subset(x = df_plot,subset = par == "mu"),
              mapping = aes(x=x,y=y,
                            group=simu_idx),alpha=0.5) +
    geom_line(data = df_mu_0,
              mapping = aes(x=x,y=y),col="blue") +
    labs(x=expression(mu),y=expression(f[mu]*(mu*'|D'))) +
    theme_bw() +
    facet_wrap( ~ I)
  
  # print(g_mu)
  
  # plot for sigma
  g_sigma <- 
    ggplot() +
    geom_line(data = subset(x = df_plot,subset = par == "sigma"),
              mapping = aes(x=x,y=y,
                            group=simu_idx),alpha=0.5) +
    geom_vline(xintercept = 0.5,col="blue") +
    labs(x=expression(sigma),y=expression(f[sigma]*(sigma*'|D'))) +
    theme_bw() +
    facet_wrap( ~ I)
  
  # print(g_sigma)
  
  # enlever les simu qui contiennent des valeurs plus grandes que 10
  df_lambda_subset <- 
    subset(x = df_plot,subset = (par == "lambda") & (y>10))
  simu_remove <- unique(df_lambda_subset$simu_idx)
  df_lambda_subset <- 
    subset(x = df_plot,subset = (par == "lambda") & !(simu_idx %in% simu_remove))
  # nice, marche bien
  
  # plot for lambda
  g_lambda <- 
    ggplot() +
    geom_line(data = df_lambda_subset,
              mapping = aes(x=x,y=y,
                            group=simu_idx),alpha=0.5) +
    geom_vline(xintercept = 1,col="blue") +
    ylim(0,5) +
    labs(x=expression(lambda),y=expression(f[lambda]*(lambda*'|D'))) +
    theme_bw() +
    facet_wrap( ~ I )
  
  # print(g_lambda)
  
  # save in list 
  list_plots[[i]] <- g_all
  
  # also plot
  g_all <- cowplot::plot_grid(g_mu,g_sigma,g_lambda,
                              ncol=1,align='v')
  
  print(g_all)
  
}

dev.off()


#'
#' After visual inspection, display index corresponding to i=15
#'


pdf(file = 'g_site-specific_one.pdf',width = 5,height = 5)
# print(list_plots[[15]]) # la liste ne marche pas for some reason
print(g_all)
dev.off()



