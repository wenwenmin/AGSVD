library(ggplot2)
library(ggpubr)
# Dat.Acc,Dat.Sensitivity, Dat.specificity
load("syntheticResultFinal.RData")
getFigs = function(Dat){
  # library(ggplot2)
  # library(ggpubr)
  Dat.Acc = Dat$Dat.Acc
  Dat.Sensitivity = Dat$Dat.Sensitivity
  Dat.specificity = Dat$Dat.specificity
  
  Dat.Acc = Dat.Acc[which(Dat.Acc$gama%in%c(0.01,0.8,1)==F),]
  Dat.Sensitivity = Dat.Sensitivity[which(Dat.Sensitivity$gama%in%c(0.01,0.8,1)==F),]
  Dat.specificity = Dat.specificity[which(Dat.specificity$gama%in%c(0.01,0.8,1)==F),]
  
  p1 = ggplot(Dat.Acc, aes(x=gama, y=accuracy, fill=method)) +  geom_bar(stat="identity", color="white", position=position_dodge()) +
    geom_errorbar(aes(ymin=accuracy-sd, ymax=accuracy+sd), width=.2, position=position_dodge(.9)) 
  p1 = p1 + coord_cartesian(ylim=c(0.6,1))+ theme_classic()  
  # p1 = p1 + theme(legend.position = c(0.3, 0.3), legend.direction = "horizontal",legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.3,"cm"))
  p1 = p1 + theme(legend.position="top", legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.3,"cm") )
  p1 = p1 #+ theme(axis.title.x=element_blank(),axis.text.x = element_blank())
  
  p2 = ggplot(Dat.Sensitivity, aes(x=gama, y=sensitivity, fill=method)) +  geom_bar(stat="identity", color="white", position=position_dodge()) +
    geom_errorbar(aes(ymin=sensitivity-sd, ymax=sensitivity+sd), width=.2, position=position_dodge(.9)) 
  p2 = p2 + theme_classic() + theme(legend.position = "none") + scale_y_continuous(breaks=c(0,0.5, 0.9))
  p2 = p2 #+ theme(axis.title.x=element_blank(),axis.text.x = element_blank())
  
  p3 = ggplot(Dat.specificity, aes(x=gama, y=specificity, fill=method)) +  geom_bar(stat="identity", color="white", position=position_dodge()) +
    geom_errorbar(aes(ymin=specificity-sd, ymax=specificity+sd), width=.2, position=position_dodge(.9)) 
  p3 = p3 + coord_cartesian(ylim=c(0.6,1)) +  theme_classic() + theme(legend.position = "none") 
  
  # color
  p1 = p1 + theme( axis.title.y = element_text(color="black", size=15)) + scale_y_continuous(breaks=c(0.6,0.8, 1)) +
    font("y.text", color="black", size = 12)+ scale_fill_manual(values = c("#FC4E07","#00AFBB","#E7B800"))
  
  p2 = p2 + theme( axis.title.y = element_text(color="black", size=15)) + 
    font("y.text", color="black", size = 12)+ scale_fill_manual(values = c("#FC4E07","#00AFBB","#E7B800"))
  
  p3 = p3 + theme( axis.title.y = element_text(color="black", size=15)) + scale_y_continuous(breaks=c(0.6,0.8, 1)) +
    font("y.text", color="black", size = 12) + scale_fill_manual(values = c("#FC4E07","#00AFBB","#E7B800"))
  p3 = p3 + theme( axis.title.x = element_text(color="black", size=15))+font("x.text", color="black", size = 12) 
  
  
  p1 = p1 + xlab(expression(gamma))
  p2 = p2 + xlab(expression(gamma))
  p3 = p3 + xlab(expression(gamma))
  
  return(figs = list(p1=p1,p2=p2,p3=p3))
}

PC1Figs = getFigs(PC1Res)
PC2Figs = getFigs(PC2Res)
ggarrange(PC1Figs$p1, PC2Figs$p1, PC1Figs$p2, PC2Figs$p2, PC1Figs$p3, PC2Figs$p3, labels = c("A", "B"), ncol = 2, nrow = 3)
ggsave("Figure4.png", width = 15, height = 15, units = "cm")