print(getwd())
library(ggplot2)
library(ggpubr)
library(ggrepel)

get_fig = function(ResDat){
  # Input ResDat 
  # plot scatterplots
  lim0 = max(c(ResDat$AGPCA,ResDat$SPCA))
  p = ggplot(ResDat, aes(x=SPCA, y=AGPCA)) + geom_point(shape=1, color="blue")  
  p = p + geom_abline(linetype = 2, colour = "red", size = 1.2, intercept = 0, slope = 1)
  p = p + geom_hline(yintercept=-log10(0.05),linetype="dashed", color = "red")+
    geom_vline(xintercept=-log10(0.05),linetype="dashed", color = "red")
  p = p + xlim(0, lim0) + ylim(0, lim0) + theme_bw()
}

# "camp1","pollen","patel","yan")
p1 = get_fig(Res[[1]]$ResDat) + ggtitle("camp1")
p2 = get_fig(Res[[2]]$ResDat) + ggtitle("pollen")
p3 = get_fig(Res[[3]]$ResDat) + ggtitle("patel")
p4 = get_fig(Res[[4]]$ResDat) + ggtitle("yan")

ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1)
ggsave("F8_figs/Raw Figure 8.png", width = 25, height = 6.8, units = "cm")