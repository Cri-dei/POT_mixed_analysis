###########  PAPER QQ ########
#### CODE FOR DAILY DISCHARGE POT ##########
######Author: Cristina Deidda ##########


#################################################################

######## IN THIS CODE ARE ################# 

rm(list = ls())


############################################################
##### Enjoy :) #############

library(gridExtra)
library(date)
library(lubridate)
library(tidyverse)
library(gtools)
library(geosphere)
library(trend)
library(plotly)
library(ggpubr)
library(viridis)
library(lattice)
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(sf)
library(maps)       # Provides functions that let us plot the maps
library(mapdata)
library(mapproj)
library(ggplot2)
library(cowplot)

# CHOOSE DIRECTORY

#pc ufficio

path<-c("C:/PROJECTS 2021/QQ_POT_mixed")
setwd(path)

# Load initial data

load("./Results/Final_matrix_POTMix_lag3.RData")
#load("./Results/POT_Lista.RData")

pv_th<-0.01
#pv_th<-0.005

POT_matrix$DEP_12<-ifelse(POT_matrix$POT_pvalue_12<=pv_th,2,1)
POT_matrix$DEP_21<-ifelse(POT_matrix$POT_pvalue_21<=pv_th,2,1)

nrow(POT_matrix)
na_pos<-which(is.na(POT_matrix[,1])==TRUE)

POT_matrix_notna<-POT_matrix[-na_pos,]

nrow(POT_matrix_notna)
100*length(which(POT_matrix_notna$Status=="Both DEP"))/nrow(POT_matrix_notna)
100*length(which(POT_matrix_notna$Status=="Both IND"))/nrow(POT_matrix_notna)
100*length(which(POT_matrix_notna$Status=="Not equal"))/nrow(POT_matrix_notna)

Summary<-data.frame(matrix(NA,2,3))
colnames(Summary)<-c("Both Dependent","Both Independent","Not symmetric")
rownames(Summary)<-c("Number Couples", "Percentages [%]")

Summary$`Both Dependent`[1]<-length(which(POT_matrix_notna$Status=="Both DEP"))
Summary$`Both Independent`[1]<-length(which(POT_matrix_notna$Status=="Both IND"))
Summary$`Not symmetric`[1]<-length(which(POT_matrix_notna$Status=="Not equal"))

Summary$`Both Dependent`[2]<-round(100*length(which(POT_matrix_notna$Status=="Both DEP"))/nrow(POT_matrix_notna),2)
Summary$`Both Independent`[2]<-round(100*length(which(POT_matrix_notna$Status=="Both IND"))/nrow(POT_matrix_notna),2)
Summary$`Not symmetric`[2]<-round(100*length(which(POT_matrix_notna$Status=="Not equal"))/nrow(POT_matrix_notna),2)

POT_matrix$Equality<-rowSums(POT_matrix[,c("DEP_12","DEP_21")])

POT_matrix$Status<-ifelse(POT_matrix$Equality==2,"Both IND",
                          ifelse(POT_matrix$Equality==4,"Both DEP","Not equal")) 

#dISTANCE IN KM
POT_matrix$Distance<-POT_matrix$Distance/1000

#################################### PLOT  ###########################################

#setwd("./Plot")
#setwd(paste0("./Plot/Pv_",pv_th))

setwd(paste0(path,"/Plot/Pv_",pv_th))
## Summary

png(paste0("Summary_pv_",pv_th,".png"), height =100, width = 220*length(Summary))
grid.table(Summary, cols = colnames(Summary), row= rownames(Summary))
dev.off()

# Kendall's Tau 12 - Kendall's Tau 21

KT_All<- ggplot(data=POT_matrix, aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Status),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))


ggsave("Mixed_Kendalltau.jpeg", units="in",dpi=400, height=7,width =10)

# Synchrony 12 - Synchrony 21

ggplot(data=POT_matrix, aes(x=Syn_21,y= Syn_12))+
  geom_point(aes(color = Status),alpha = 0.3) + theme_bw()+
  labs(x = "Synchrony 21", y="Synchrony 12")+ 
  theme(legend.text=element_text(size=10))

ggsave("Mixed_Syncrony.jpeg", units="in",dpi=400, height=7,width =10)


#################################


ALL_DEP<- ggplot(data=POT_matrix[which(POT_matrix$Status=="Both DEP"),], aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Status),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+
  scale_color_manual(values=c("#CC79A7"))


ggsave("01.KT_ALLDEP.jpeg", units="in",dpi=400, height=7,width =10)


ALL_IND<- ggplot(data=POT_matrix[which(POT_matrix$Status=="Both IND"),], aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Status),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+
  scale_color_manual(values=c("#009E73"))


ggsave("02.KT_ALLIND.jpeg", units="in",dpi=400, height=7,width =10)


ALL_NOTEQ<- ggplot(data=POT_matrix[which(POT_matrix$Status=="Not equal"),], aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Status),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+
  scale_color_manual(values=c("#56B4E9"))


ggsave("02.KT_NOTEQUAL.jpeg", units="in",dpi=400, height=7,width =10)


GG_classes<-ggarrange(KT_All,ALL_DEP,ALL_IND, ALL_NOTEQ,ncol=2,nrow=2, labels=c("a)","b)","c)","d)") ,font.label = list(size = 12)) 

GG_classes


ggsave("KT_classes.jpeg", units="in",dpi=400, height=7,width =12)

###################################

# Varying with distance



KT_All_dist<- ggplot(data=POT_matrix, aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Distance),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+ 
  scale_color_gradientn(colours = rainbow(10))+
  ggtitle("ALL")


ALL_DEP_dist<- ggplot(data=POT_matrix[which(POT_matrix$Status=="Both DEP"),], aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Distance),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+ 
  scale_color_gradientn(colours = rainbow(10))+
  ggtitle("Dependent couples")



#ggsave("01.KT_ALLDEP.jpeg", units="in",dpi=400, height=7,width =10)


ALL_IND_dist<- ggplot(data=POT_matrix[which(POT_matrix$Status=="Both IND"),], aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Distance),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+
  scale_color_gradientn(colours = rainbow(10))+
  ggtitle("Independent couples")


#ggsave("02.KT_ALLIND.jpeg", units="in",dpi=400, height=7,width =10)


ALL_NOTEQ_dist<- ggplot(data=POT_matrix[which(POT_matrix$Status=="Not equal"),], aes(x=POT_KT_21,y= POT_KT_12))+
  geom_point(aes(color = Distance),alpha = 0.3) + theme_bw()+
  labs(x = "Kendall's Tau 21", y="Kendall's Tau 12")+ 
  theme(legend.text=element_text(size=10))+
  scale_color_gradientn(colours = rainbow(10))+
  ggtitle("Asymmetric dependent couples")


#ggsave("02.KT_NOTEQUAL.jpeg", units="in",dpi=400, height=7,width =10)


GG_classes_dist<-ggarrange(KT_All_dist,ALL_DEP_dist,ALL_IND_dist, ALL_NOTEQ_dist,ncol=2,nrow=2, 
                           labels=c("a)","b)","c)","d)") ,font.label = list(size = 12),common.legend = TRUE,
                           legend="bottom") 

GG_classes_dist


ggsave("KT_DISTANCE_classes.jpeg", units="in",dpi=400, height=7,width =10)

#######################################################################

