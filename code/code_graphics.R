# Load R package -------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(here)

# plot ---------------------------------------------------------------- 

frecuency_betweenness_degree=read.table(here("data","abundance_betweenness_degree.csv"), header=TRUE, sep=",")  
names(frecuency_betweenness_degree)
ggplot(data = frecuency_betweenness_degree)
graf1=ggplot(data =frecuency_betweenness_degree, aes(x = abundance, y = betweenness)) + geom_point( size = 3)+ggrepel::geom_label_repel(aes(label = Species),size = 7, data = frecuency_betweenness_degree)
theme_set(theme_gray(base_size = 24))
g1=graf1+ labs(x = "Abundance", y = "Betweenness") + theme(plot.title = element_text(size=24))   #plot abundance vs betweennees 
g1


ggplot(data = frecuency_betweenness_degree)
graf2=ggplot(data =frecuency_betweenness_degree, aes(x =degree, y = betweenness)) + geom_point(size=3) +ggrepel::geom_label_repel(aes(label = Species),size = 7, data = frecuency_betweenness_degree)+
theme_set(theme_gray(base_size = 24))
g2=graf2+ labs(x = "Degree", y = "Betweenness") + theme(plot.title = element_text(size=24))  #plot degree vs betweennees 
g2

myplot1 <- arrangeGrob(g1, top = textGrob("B", x = unit(0, "npc")
                                          , y   = unit(1, "npc"), just=c("left","top"),
                                          gp=gpar(col="black", fontsize=24, fontfamily="Times Roman")))

myplot2 <- arrangeGrob(g2, top = textGrob("A", x = unit(0, "npc")
                                          , y = unit(1, "npc"), just=c("left","top"),
                                          gp=gpar(col="black", fontsize=24, fontfamily="Times Roman")))
grid.arrange(myplot2, myplot1, nrow = 1)


# Correlation between degree and betweenness, abundance and betweenness --------------------------------------------------


cor.test(frecuency_betweenness_degree$betweenness,frecuency_betweenness_degree$degree,method = "spearman")
cor.test(frecuency_betweenness_degree$betweenness,frecuency_betweenness_degree$abundance,method = "spearman")


# modularity graph --------------------------------------------------------

modularityvalidation=read.table(here("data","modularityx10.csv"), header=TRUE, sep=",") 
averaging<- modularityvalidation%>%group_by(change)%>%summarise_all(mean)
#View(averaging)

ggplot(data =averaging)
highlight_df <- averaging %>% filter(v==v[1])

grafmod=ggplot(data =averaging, aes(x =change, y =v)) + geom_point( size=3) +  geom_point(data=highlight_df, 
                                                                                        aes(x=change,y=v), 
                                                                                        fill="blue", color="darkred",
                                                                                        size=3.5,shape=23)+
theme_set(theme_gray(base_size = 24))
grafmod + labs(x = "Change", y = "Modularity") + theme(plot.title = element_text(size=24))  

# Histogram --------------------------------------------------------------

outdegreeori=read.table(here("data","outdegree.csv"), header=TRUE, sep=",") 

ggplot(data=outdegreeori, aes(Outdegree)) + 
  geom_histogram(breaks=seq(0,9, by=1), col="darkmagenta", 
                 fill="blueviolet", 
                 alpha = .2) +labs(x="Degree", y="Count") + 
  xlim(c(0,10))+ 
  ylim(c(0,10))+theme(text= element_text(size=24)) 


### Plot Betweenness vs Order
betweenness_order<-sort(frecuency_betweenness_degree$betweenn/max(frecuency_betweenness_degree$betweenn),decreasing = TRUE)
orden<-c(1:11)

matrix_betweenness<-data.frame(cbind(orden,betweenness_order))

ggplot(data =matrix_betweenness, aes(x =orden, y = betweenness_order)) + 
  geom_point(size=3) +geom_line()+
  theme_set(theme_gray(base_size = 24))+ labs(x = "Order", y = "Betweenness") + 
  theme(plot.title = element_text(size=24))  #plot  betweennees vs order 
