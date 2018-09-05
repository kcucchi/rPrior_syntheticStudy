
#'
#' plot with other figure
#'

g_one_312 <- readRDS(file = 'g_one_312.rds')
g_all_312 <- readRDS(file = 'g_all_312.rds')

dev.off()
pdf(file = 'g_312.pdf',width = 8,height=3)
print(cowplot::plot_grid(g_one_312,
                         g_all_312 +
                           guides(fill =
                                    guide_legend(
                                      nrow=2
                                    )),
                         labels = c('a','b')))
dev.off()

