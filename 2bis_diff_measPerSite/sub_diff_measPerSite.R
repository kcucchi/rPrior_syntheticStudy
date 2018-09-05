#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(rPrior)

if (length(args) < 1) {
  print(length(args))
  print(args[1])
  stop("Two arguments must be provided", call.=FALSE)
}

i_row <- args[1]

write(paste0('in subfile with i_row=',i_row),
      file="log.txt",append=TRUE)

# load measurements and results dataframe
Y_meas <- readRDS(file = 'Y_meas.rds')
df_res <- readRDS(file = 'df_res_sec3.1.2.rds')

# subset to relevant measurements only
Y_meas_irow <- subset(Y_meas,simu_idx == df_res[i_row,'simu_idx'])

# check correspondance between setup and sampled measurements
# Y_meas_irow
# df_res[i_row,]

# x vector for densities
vect_x <- seq(from = -15,
              to = 0,
              by = 0.1)

# for both sites
res_rPrior_12 <- 
  rPrior::regionalized(data = Y_meas_irow,
                       eval_theta = vect_x)

# for site S_idx1 only
res_rPrior_1 <- 
  rPrior::regionalized(data = subset(Y_meas_irow,site_id == df_res[i_row,'S_idx1']),
                       eval_theta = vect_x)


# for site S_idx2 only
res_rPrior_2 <- 
  rPrior::regionalized(data = subset(Y_meas_irow,site_id == df_res[i_row,'S_idx2']),
                       eval_theta = vect_x)

# as if data was coming from the same site
Y_meas_sameSite <- Y_meas_irow;Y_meas_sameSite$site_id = 'S1'
res_rPrior_sameSite <- 
  rPrior::regionalized(data = Y_meas_sameSite,
                       eval_theta = vect_x)

#'
#' plot to check
#'

# # put all in same dataframe
# df_rPrior_res <- 
#   rbind(data.frame(res_rPrior_12$uPrior,
#                    site="non informed"),
#         data.frame(res_rPrior_1$rPrior,
#                    site = df_res[i_row,'S_idx1']),
#         data.frame(res_rPrior_2$rPrior,
#                    site = df_res[i_row,'S_idx2']),
#         data.frame(res_rPrior_12$rPrior,
#                    site = paste(df_res[i_row,c('S_idx1','S_idx2')],collapse = '+')),
#         data.frame(res_rPrior_sameSite$rPrior,
#                    site = 'same site'))

# ggplot() +
#   geom_line(data = df_rPrior_res,
#             mapping = aes(x=x,y=y,col=site)) +
#   theme_bw()

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

# df_res[i_row,]

saveRDS(object = df_res,file = 'df_res_sec3.1.2.rds')
