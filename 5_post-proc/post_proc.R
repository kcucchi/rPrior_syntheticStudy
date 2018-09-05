

library(ggplot2)
library(dplyr)

#'
#' # Load data #
#'

# read results dataframe
df_results <- readRDS('../4_calc_res/df_results_kld.rds')
nrow(df_results)

# get rid of NA values
df_results <- subset(x = df_results,subset = !is.na(kld))
nrow(df_results)

# join with parameters
df_setup <-
  read.csv(file = '../0_main/2_param_meas.csv',
           sep=';')

# add parameters of simulation for each meas_idx/real_idx combination
df_results <- 
  dplyr::left_join(
    x = df_results,y = df_setup,
    by="meas_idx")

# replace "meas" by "measurements"
df_results$type <- 
  gsub(pattern = "^meas",
       replacement = "measurements",
       x = df_results$type)

#'
#' ## a bit of stats: calculate median and quartiles for each combination ##
#'

# df_results_sumd <-
#   df_results %>%
#   group_by(I,J) %>%
#   summarize(median=median(kld,na.rm=T),
#             low=quantile(kld,probs=0.25,na.rm=T),
#             high=quantile(kld,probs=0.75,na.rm=T))

#'
#' # plot results #
#'

#'
#' ## First do I,J ##
#'

# fix r to 10 and type to "meas"
df_results_IJ <- 
  subset(x = df_results,
         subset = (variable == "Y") & (r==10) & (type=="measurements") & (J!=20))

ggplot(data=df_results_IJ) +
  geom_boxplot(mapping = aes(x=factor(I),y=kld,fill=factor(J))) +
  labs(x = "I", y = "KLD", fill = 'J') +
  scale_y_log10() +
  theme_bw()

g_KLD_IJ <-
  ggplot(data=df_results_IJ) +
  # geom_point(mapping = aes(x=I,y=kld,col=factor(J)),alpha=0.5) +
  geom_smooth(mapping = aes(x=I,y=kld,col=factor(J)),method = "loess") +
  labs(x = "I", y = "KLD", col = 'J') +
  scale_y_log10(breaks=c(0.01,0.025,0.05,0.075,
                         0.1,0.25,0.5,0.75,1,2.5,5,10)) +
  theme_bw()

g_KLD_IJ

pdf('g_KLD_IJ.pdf',width = 5,height = 3)
print(g_KLD_IJ)
dev.off()

#'
#' same figure with mu
#'

df_results_IJ_mu <- 
  subset(x = df_results,
         subset = (variable == "mu") & (r==10) & (type=="measurements") & (J!=20))

g_KLD_IJ_mu <-
  ggplot(data=df_results_IJ_mu) +
  # geom_point(mapping = aes(x=I,y=kld,col=factor(J)),alpha=0.5) +
  geom_smooth(mapping = aes(x=I,y=kld,col=factor(J)),method = "loess") +
  labs(x = "I", y = "KLD", col = 'J') +
  scale_y_log10(breaks=c(0.01,0.025,0.05,0.075,
                         0.1,0.25,0.5,0.75,1,2.5,5,10,
                         25,50,75,100)) +
  theme_bw()

g_KLD_IJ_mu

pdf('g_KLD_IJ_mu.pdf',width = 5,height = 3)
print(g_KLD_IJ_mu)
dev.off()

# plot Y and mu in same figure

dev.off()
pdf(file = 'g_313.pdf',width = 8,height=3)
print(cowplot::plot_grid(g_KLD_IJ +
                           labs(y='KLD(y)')+
                           theme(legend.position='top'),
                         g_KLD_IJ_mu +
                           labs(y=bquote('KLD('*mu*')'))+
                           theme(legend.position='top'),
                         labels = c('a','b')))
dev.off()


#'
#' ## Now do I,r ##
#'

#'
#' First without the spatial correlation model
#'

# fix r to 10 and type to "meas"
df_results_Ir <- 
  subset(x = df_results,subset = (type=="measurements")&(J==30))

ggplot(data=df_results_Ir) +
  geom_boxplot(mapping = aes(x=factor(I),y=kld,fill=factor(r))) +
  labs(x = "I", y = "KLD", fill = 'r') +
  scale_y_log10() +
  theme_bw()

g_KLD_Ir <-
  ggplot(data=df_results_Ir) +
  # geom_point(mapping = aes(x=I,y=kld,col=factor(J)),alpha=0.5) +
  geom_smooth(mapping = aes(x=I,y=kld,col=factor(r)),method = "loess") +
  labs(x = "I", y = "KLD", col = expression(r/lambda)) +
  scale_y_log10(breaks=c(0.01,0.025,0.05,0.075,
                         0.1,0.25,0.5,0.75,1,2.5,5,10)) +
  theme_bw()

g_KLD_Ir

pdf('g_KLD_Ir.pdf',width = 5,height = 3)
print(g_KLD_Ir)
dev.off()

#'
#' Now with the spatial correlation model
#'

# fix r to 10 and type to "meas"
df_results_Ir_spatial <- 
  subset(x = df_results,
         subset = (type %in% c("measurements","measurements_spatial") & J==30))

ggplot(data=df_results_Ir_spatial) +
  geom_boxplot(mapping = aes(x=factor(I),y=kld,fill=factor(r))) +
  labs(x = "I", y = "KLD", fill = 'r') +
  scale_y_log10() +
  theme_bw() +
  facet_wrap( ~ type)

g_KLD_Ir_spatial <-
  ggplot(data=df_results_Ir_spatial) +
  # geom_point(mapping = aes(x=I,y=kld,col=factor(J)),alpha=0.5) +
  geom_smooth(mapping = aes(x=I,y=kld,col=factor(r),linetype=type),method = "loess") +
  labs(x = "I", y = "KLD", 
       col = expression(r/lambda),
       linetype='spatial\nautocorrelation') +
  scale_linetype_discrete(labels=c('not accounting',
                                   'accounting')) +
  scale_y_log10(breaks=c(0.01,0.025,0.05,0.075,
                         0.1,0.25,0.5,0.75,1,2.5,5,10)) +
  theme_bw() 

g_KLD_Ir_spatial

table(df_results_Ir_spatial[c('I','r')])

pdf('g_KLD_Ir_spatial.pdf',width = 5,height = 3)
print(g_KLD_Ir_spatial)
dev.off()



#'
#' # Comparison with type of measurements #
#'

# select r to be 10 (minimize effect of spatial correlation)
# select J to have several values
# (check effect of number of measurements for computing measurements statistics)

df_results_IType <- 
  subset(x = df_results,subset = (r==10) & (J!=20))

g_KLD_IType <-
  ggplot(data=df_results_IType) +
  geom_smooth(mapping = aes(x=I,y=kld,color=type),method = "loess") +
  labs(x = "I", y = "KLD", col = "data type") +
  scale_y_log10(breaks=c(0.01,0.025,0.05,0.075,
                         0.1,0.25,0.5,1,2.5,5,10)) +
  facet_wrap(~ J,
             labeller = label_both,nrow = 1) +
  theme_bw() 

g_KLD_IType

pdf('g_KLD_IType.pdf',width = 10,height = 2.5)
print(g_KLD_IType)
dev.off()

