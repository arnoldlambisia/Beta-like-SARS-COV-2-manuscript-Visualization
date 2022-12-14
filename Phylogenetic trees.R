#ARNOLD LAMBISIA
##Copyright 2022 #version1.

#thus script is for visualizing and maniplating phylogentic trees


rm(list=ls())
#load packages
library(tidyverse); library(lubridate); library(janitor); library(artyfarty);library(ggalluvial);library(ggrepel);library(ggtree); 
library(tidytree);library(treeio); library(wesanderson); library(janitor);library(rio); library(ggpubr);library(phylotools); library(patchwork); library(scales)

#specify colrs of interest to be used
color_global=c("cadetblue","magenta","purple4","tan4","firebrick4",
               "blue","orange","yellow","cyan","pink","darkolivegreen4",
               "wheat","lightsalmon","gold","red",
               "mediumorchid1","chartreuse","wheat","blue","darkgray",
               "darkblue","#FFFF00","#3E2723","#00FF00","#008000")

###plot a treetime
#load tree
tree1<-read.tree('tree1.nwk')

#load metadata
meta_tree1 <- import("dataset.txt")

#visulaizee tree
tree1_plot <- ggtree(tree1, color='grey40', size=0.3)
  theme_tree2()+
  theme(axis.text.x = element_text(size=10,angle=0))

tree1_plot

#color tree based on pango lineage
plot_facility_VP4 <-tree1_plot%<+% meta_tree1+  
  geom_tippoint(aes(color=Lineage), size = 2.5, shape = 15, show.legend = T)+ 
  scale_shape_manual(values=c(16, 15, 18, 17, 14))+
  scale_color_manual(values=c(color_global,"#ef8a62", "blue", "orange", wes_palette(5, name = "Darjeeling1", type = "discrete"),"black",
                              wes_palette(5, name = "Cavalcanti1", type = "discrete"),terrain.colors(2),"khaki","gray77",rainbow(10)[c(-1,-2,-4)],
                              as.character(wes_palette(4, name = "Royal1", type = "discrete")),as.character(wes_palette(5, name = "Zissou1", type = "discrete")),
                              "pink","gray32","tomato1", color_global))+
   guides(shape=guide_legend(ncol=1, title = "Status", title.position = "top", title.theme = element_text(size=12, face="bold"), order=1),
          color=guide_legend(ncol=1, title = "Lineage", title.position = "top", title.theme = element_text(size=12, face="bold")))
 
plot_tree1_divergence


##plot a treetime
#load tree
tree_divergence<-read.nexus('divergence_tree.nexus')
#load metadata
all_meta_2 <- import("Metadata.xlsx")

#visualize tree using ggtree
p_divergence <- ggtree(tree_divergence, color='grey40', size=0.3) 
  theme_tree2()+
  theme(axis.text.x = element_text(size=10,angle=0))

p_divergence

#color tree based on status
plot_diverence_beta <-  p_divergence%<+% all_meta_2+
  geom_tippoint(aes(subset=(country=="Kenya"), color=Status), size=4)+
  scale_shape_manual(values=c( 15, 16, 18, 17, 14))+
  scale_color_manual(values=c("red", "#ef8a62", "orange", wes_palette(5, name = "Darjeeling1", type = "discrete"),"black",
                              wes_palette(5, name = "Cavalcanti1", type = "discrete"),terrain.colors(2),"khaki","gray77",rainbow(10)[c(-1,-2,-4)],
                              as.character(wes_palette(4, name = "Royal1", type = "discrete")),as.character(wes_palette(5, name = "Zissou1", type = "discrete")),
                              "pink","gray32","tomato1", color_global))+
  theme(rect = element_blank(),
        legend.position = c(0.2, 0.8))+
  guides(shape=guide_legend(ncol=1, title = "Status", title.position = "top", title.theme = element_text(size=12, face="bold"), order=1),
         color=guide_legend(ncol=1, title = "Status", title.position = "top", title.theme = element_text(size=12, face="bold")))


plot_diverence_beta



##plot a treetime
#load tree
tree_treetime<-read.nexus('timetree.nexus')

#set the most recent date
mr <- as.Date("2022-09-26")

#visualize tree using ggtree
p_treetime<-ggtree(tree_treetime, mrsd=mr, as.Date=TRUE,size=0.3, ladderize=TRUE) + theme_tree2()+
  scale_x_date(date_labels = "%b-%y",date_breaks = "6 months")
  theme_tree2(legend.position='left')+
  theme(axis.text.x = element_text(size=20,angle=90))
p_treetime

#color Kenyan tips bases on status
plot_treetime_beta <-  p_treetime%<+% all_meta_2+
  geom_tippoint(aes(subset=(country=="Kenya"), color=Status), size=4, show.legend = F)+
  scale_shape_manual(values=c( 15, 16, 18, 17, 14))+
  scale_color_manual(values=c("red", "#ef8a62", "orange", wes_palette(5, name = "Darjeeling1", type = "discrete"),"black",
                              wes_palette(5, name = "Cavalcanti1", type = "discrete"),terrain.colors(2),"khaki","gray77",rainbow(10)[c(-1,-2,-4)],
                              as.character(wes_palette(4, name = "Royal1", type = "discrete")),as.character(wes_palette(5, name = "Zissou1", type = "discrete")),
                              "pink","gray32","tomato1", color_global))+
  theme(legend.position = c(0.2, 0.8),
        axis.text.x = element_text(size = 12, angle = 90))+
  guides(shape=guide_legend(ncol=1, title = "Status", title.position = "top", title.theme = element_text(size=12, face="bold"), order=1),
         color=guide_legend(ncol=1, title = "Status", title.position = "top", title.theme = element_text(size=12, face="bold")))


plot_treetime_beta





