#'
#'---
#'title: comparison with different number of measurements per site
#'author: Karina Cucchi
#'date: May 6 2018
#'---
#'

#'
#' In this script I compare the regionalized prior 
#' for a different number of measurements per site. 
#' We consider 2 sites only.
#' J_1 is the number of measurements at site S1.
#' J_2 is the number of measurements at site S2.
#' We use KLD between an uninformative prior and the derived regionalized prior
#' as a measure of information gain.
#' KLD_1 is the gain in information using site S1 only.
#' KLD_2 is the gain in information using site S2 only.
#' KLD_12 is the gain in information using sites S1 and S2.
#' KLD_same is the gain in information using information from both sites, 
#' but ignoring inter-site variability 
#' (i.e. considering that all measurements come from the same site). 
#' a is KLD_1/KLD_12. 
#' b is KLD_2/KLD_12. 
#'


library(ggplot2)

df_res <- readRDS(file = 'df_res_sec3.1.2.rds')

# View(df_res)

# select rows without NAs only
df_res <- subset(x = df_res,subset = !is.na(KLD_1))

#'
#' # first plot distribution for J_1=2 and J_2=5 only #
#'  

df_res_subset <- subset(x = df_res,
                        subset = (J_1 == 2 & J_2 == 5))

# df_res_subset

# switch to long format
df_res_subset_long <- 
  reshape2::melt(data = df_res_subset,
                 id = c("J_1","J_2", "real_idx", "S_idx1", "S_idx2","simu_idx"))

# ggplot(df_res_subset_long) +
#   geom_density(mapping = aes(x = value,fill=variable),alpha=0.5) +
#   theme_bw()


ggplot(subset(df_res_subset_long,variable %in% c('KLD_1','KLD_2','KLD_12','KLD_same'))) +
  geom_boxplot(mapping = aes(x = variable,y=value)) +
  theme_bw()


#'
#' This figure shows that the gain in information is always much higher 
#' when using 2 sites instead of 1. 
#' KLD_1, KLD_2, and KLD_same are on the same order of magnitude.
#'

# ggplot(subset(df_res_subset_long,variable %in% c('a','b'))) +
#   geom_density(mapping = aes(x = value,fill=variable),alpha=0.5) +
#   theme_bw()

ggplot(subset(df_res_subset_long,variable %in% c('a','b'))) +
  geom_boxplot(mapping = aes(x = variable,y=value)) +
  theme_bw()

#'
#' This figure compares the distribution of a = KLD_1/KLD_12 and b = KLD_2/KLD_12. 
#' The first observation is that, on average, b is higher than a. 
#' This suggests that the regionalized prior algorithm recognizes that 
#' site S_2 (containing 5 measurements) 
#' contains on average more information than 
#' site S_1 (containing 2 measurements). 
#' The second observation is that the distributions of a and b largely overlap. 
#' This suggests that adding measurements at a given site
#' leads to second order improvements when compared to 
#' adding a new site.
#'
#'

#'
#' # Now try visualisations as a function of J_1 #
#'

#'
#' Second, we investigate the sensitivity of the results 
#' to varying number of measurements at site S_1.
#'
#'

df_res_long <- 
  reshape2::melt(data = df_res,
                 id = c("J_1","J_2", "real_idx", "S_idx1", "S_idx2","simu_idx"))

# ggplot(data = subset(df_res_long,
#                      subset = variable %in% c('KLD_1','KLD_2','KLD_12','KLD_same'))) +
#   geom_boxplot(mapping = aes(x = variable,y=value)) +
#   facet_grid(J_2 ~ J_1,labeller = label_both) +
#   theme_bw()

g_compare <-
  ggplot(data = subset(df_res_long,
                       subset = variable %in% c('KLD_1','KLD_2','KLD_12','KLD_same') & 
                         J_2 == 5)) +
  geom_boxplot(mapping = aes(x = factor(J_1),y=value,fill=factor(variable)),
               lwd=0.1) +
  scale_y_continuous(breaks=c(0.1,0.5,1,5,10),
                     trans = "log10") +
  scale_fill_discrete(labels=c('KLD_1'=bquote('S'['1']),
                               'KLD_2'=bquote('S'['2']),
                               'KLD_12'=bquote('S'['1'] *' + S'['2']),
                               'KLD_same'=bquote('S'['1'] *' U S'['2']))) +
  labs(x=bquote('J'['1']),y='KLD',fill='sites') +
  # facet_grid(J_2 ~ J_1,labeller = label_both) +
  theme_bw() +
  theme(legend.position = 'top')

g_compare

pdf(file = 'g_all_312.pdf',width = 4,height = 3)
print(g_compare)
dev.off()

saveRDS(object = g_compare,file = 'g_all_312.rds')

#'
#' The KLD is much higher for S1+S2 than for the other
#' configurations, and this is valid for all values of J_1. 
#' This suggests that there is more information to be gained 
#' by assimilating measurements from different sites, 
#' than by adding more measurements at the same site, 
#' as the KLD showed little sensitivity to the number of measurements per site
#' for all values of J_1.
#'
#'



