

library(rPriorSynthetic)
library(scales)


if(file.exists('ls_one.RData')){
  
  load(file = 'ls_one.RData')
  
}else{
  
  
  #
  #  load synthetic field sites
  #  
  
  Y_xy <- readRDS(file = '../1_fields/Y_xy_field_1.rds')
  
  #
  # # Define how many measurements we want to sample #
  #
  
  # eventually, but start simple
  # J_comb <- data.frame(expand.grid(J_1=c(2,3,4,5,7,10,12,15,17,20,25,30),
  #                                  J_2=c(2,3,4,5,7,10,12,15,17,20,25,30)))
  
  df_res <- 
    data.frame(expand.grid(J_1=2,
                           J_2=5,
                           real_idx=1,
                           S_idx1=NA,
                           S_idx2=NA,
                           KLD_1=NA,
                           KLD_2=NA,
                           KLD_12=NA,
                           KLD_same=NA,
                           a=NA,
                           b=NA))
  
  
  #
  # # Start by choosing 2 sites #
  #
  
  set.seed(1)
  
  i_row <- 1
  
  # randomly select 2 field sites
  # site_names <- paste0('S',sample(x = 1:100,size = 2))
  df_res[i_row,c('S_idx1','S_idx2')] <- c('S1','S2')
  df_res[i_row,]
  
  #
  # sample measurements from sites 
  #
  
  # I can't use rPriorSynthetic::generate_meas 
  # because it only allows for same number of measurements per site
  # so, implement it here
  
  # define parameters for measurement sampling
  vect_meas <- c(I=2,
                 J_1=df_res[i_row,'J_1'],
                 J_2=df_res[i_row,'J_2'],
                 r=10)
  
  vect_meas
  
  # this will contain measurement values at all sites
  Y_meas <-
    data.frame(id_meas=numeric(0),
               site_id=numeric(0),
               val=numeric(0),
               x=numeric(0),
               y=numeric(0))
  
  
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
      site_id = as.character(df_res[i_row,c('S_idx1','S_idx2')][i_site])
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
  
  # format Y_meas for regionalized
  names(Y_meas)[which(names(Y_meas) == "val")] <- "dat"
  Y_meas$type <- 'meas'
  
  Y_meas
  
  #
  # calculate rPrior 
  #
  
  # x vector for densities
  vect_x <- seq(from = -15,
                to = 0,
                by = 0.1)
  
  # for both sites
  res_rPrior_12 <- 
    rPrior::regionalized(data = Y_meas,
                         eval_theta = vect_x)
  
  # for site S_idx1 only
  res_rPrior_1 <- 
    rPrior::regionalized(data = subset(Y_meas,site_id == df_res[i_row,'S_idx1']),
                         eval_theta = vect_x)
  
  
  # for site S_idx2 only
  res_rPrior_2 <- 
    rPrior::regionalized(data = subset(Y_meas,site_id == df_res[i_row,'S_idx2']),
                         eval_theta = vect_x)
  
  # as if data was coming from the same site
  Y_meas_sameSite <- Y_meas;Y_meas_sameSite$site_id = 'S1'
  res_rPrior_sameSite <- 
    rPrior::regionalized(data = Y_meas_sameSite,
                         eval_theta = vect_x)
  
  save(list = ls(all.names = TRUE),file = 'ls_one.RData')
  
}

#'
#' plot to check
#'

# put all in same dataframe
df_rPrior_res <- 
  rbind(data.frame(res_rPrior_12$uPrior,
                   site="non informed"),
        data.frame(res_rPrior_1$rPrior,
                   site = df_res[i_row,'S_idx1']),
        data.frame(res_rPrior_2$rPrior,
                   site = df_res[i_row,'S_idx2']),
        data.frame(res_rPrior_12$rPrior,
                   site = paste(df_res[i_row,c('S_idx1','S_idx2')],collapse = '+')),
        data.frame(res_rPrior_sameSite$rPrior,
                   site = 'same site'))

g_one_312 <-
  ggplot() +
  geom_line(data = df_rPrior_res,
            mapping = aes(x=x,y=y,col=site)) +
  scale_color_manual(
    values=c('black',hue_pal()(4)), # 4 default colors to be consistent with boxplot figure
    labels=c('S1'=bquote('S'['1']),
             'S2'=bquote('S'['2']),
             'S1+S2'=bquote('S'['1'] *' + S'['2']),
             'same site'=bquote('S'['1'] *' U S'['2']))) +
  labs(x='y',y=bquote('f'['Y']*'(y|D)'),col='sites') +
  theme_bw() +
  theme(legend.position='top') +
  guides(col =
           guide_legend(
             nrow=2
           ))

g_one_312

pdf(file = 'g_one_312.pdf',width = 4,height = 3)
print(g_one_312)
dev.off()

saveRDS(object = g_one_312,file = 'g_one_312.rds')



#'
#'
#'
#' calculate KLD
#'

df_res[i_row,'KLD_1'] <- 
  rPrior::KL_divergence(theta = res_rPrior_1$uPrior$x,
                        p_theta = res_rPrior_1$uPrior$y,
                        q_theta = res_rPrior_1$rPrior$y)

df_res[i_row,'KLD_2'] <- 
  rPrior::KL_divergence(theta = res_rPrior_2$uPrior$x,
                        p_theta = res_rPrior_2$uPrior$y,
                        q_theta = res_rPrior_2$rPrior$y)

df_res[i_row,'KLD_12'] <- 
  rPrior::KL_divergence(theta = res_rPrior_12$uPrior$x,
                        p_theta = res_rPrior_12$uPrior$y,
                        q_theta = res_rPrior_12$rPrior$y)

df_res[i_row,'KLD_same'] <- 
  rPrior::KL_divergence(theta = res_rPrior_sameSite$uPrior$x,
                        p_theta = res_rPrior_sameSite$uPrior$y,
                        q_theta = res_rPrior_sameSite$rPrior$y)


df_res[i_row,'a'] <- df_res[i_row,'KLD_1'] / df_res[i_row,'KLD_12']
df_res[i_row,'b'] <- df_res[i_row,'KLD_2'] / df_res[i_row,'KLD_12']

df_res

