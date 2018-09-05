
#'
#' In this script I plot images 
#' for the figure presenting the synthetic study setup
#' in the regionalized prior paper
#'

library(ggplot2)
library(rPrior)

#'
#' # first step #
#'

# read setup file

df_setup <-
  read.csv(file = '../../0_main/2_param_meas.csv',
           sep=';')

#'
#' # Calculate underlying density at all sites #
#'

# for now just one underlying synthetic truth, 
# so just calculate the density at this site

Y_xy <- readRDS(file = '../../1_fields/Y_xy_field_1.rds')

# plot one of the random fields

g_setup_S1 <-
  ggplot(data=Y_xy) +
  geom_tile(mapping = aes(x=x,y=y,fill=S1)) +
  theme_void() + theme(legend.position="none")

g_setup_S1

pdf('g_setup_S1.pdf')
print(g_setup_S1)
dev.off()

# zoom (for checking)
# ggplot(data=Y_xy) +
#   geom_tile(mapping = aes(x=x,y=y,fill=S1)) +
#   xlim(0,4) + ylim(0,4)

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

g_setup_underlying <-
  ggplot(data = dens_Y_all) + 
  geom_line(aes(x,y),linetype="dashed") + 
  ggtitle("underlying density") +
  labs(x="",y="") +
  theme_bw()

pdf('g_setup_underlying.pdf',width = 3,height = 2)
print(g_setup_underlying)
dev.off()

rm(Y_xy,Y_all,vect_x,fields_idx)

#'
#' ## plot generated priors ##
#'

meas_idx = 2

df_rPrior_forPlot <- data.frame(x=numeric(0),
                                y=numeric(0),
                                real_idx=numeric(0))

for(i_real in 1:10){
  
  path2rPrior <-
    paste0('../3_rPrior/meas/meas_',meas_idx,
           '/rPrior_meas_',meas_idx,
           '_real_',i_real,'.rds')
  
  rPrior_i <- readRDS(file = path2rPrior)$rPrior
  
  df_rPrior_forPlot <- rbind(df_rPrior_forPlot,
                             data.frame(x=rPrior_i$x,
                                        y=rPrior_i$y,
                                        real_idx=i_real))
  
}

g_setup_rPrior <-
  ggplot(df_rPrior_forPlot) +
  geom_line(mapping = aes(x=x,y=y,col=factor(real_idx),group=real_idx)) +
  theme_bw() +
  ggtitle("regionalized priors") +
  theme(legend.position="none") +
  labs(x="",y="")

pdf('g_setup_rPrior.pdf',width = 3,height = 2)
print(g_setup_rPrior)
dev.off()
