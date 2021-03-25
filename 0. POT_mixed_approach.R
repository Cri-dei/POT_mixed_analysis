###########  PAPER QQ ########
#### CODE FOR DAILY DISCHARGE POT ##########
######Author: Cristina Deidda ##########


#################################################################

######## IN THIS CODE ARE ################# 

rm(list = ls())


############################################################
##### Enjoy :) #############

library(date)
library(lubridate)
library(tidyverse)
library(gtools)
library(geosphere)
library(trend)

# CHOOSE DIRECTORY

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
path<-c("C:/PROJECTS 2021/QQ")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")


#############################0. IMPORT FILE ######################################

source(paste0(path,"/Code/Functions/","Randomization.R"))
source(paste0(path,"/Code/Functions/","POT_annualmax.R"))
source(paste0(path,"/Code/Functions/","POT_monthly_variable_max.R"))
source(paste0(path,"/Code/Functions/","POT_eventmix_12.R"))
source(paste0(path,"/Code/Functions/","POT_eventmix_21.R"))

# Load daily data

load("C:/PROJECTS 2021/QQ_POT_mixed/Data/Daily_data.RData")



# Import geographical info

setwd(paste0(path,"/Data"))
Data_INFO<-read.csv("Catch_info_final.csv",sep=";")
M_coord<- read.table("nrfa-coords3.csv", header = TRUE, sep=",")    #table excel with value

### Set English language for date format

Sys.setlocale('LC_ALL','en_CA.utf-8');
Sys.setlocale('LC_ALL','English');

#Import POT FILE

###### File available #########

setwd(paste0(path,"/Data/POT_data/Suitable"))

File_st<-list.files(path = ".")
Available_st<-sapply(1:length(File_st),function(x){as.numeric(str_split(File_st[x], "\\.")[[1]][1])})
Available_st<-unique(Available_st)


############# All combination between stations ########

Allcomb<-data.frame(combinations(length(Available_st), 2, v=Available_st, set=TRUE, repeats.allowed=FALSE))
colnames(Allcomb)<-c("ID_Station_1","ID_Station_2")

comb_perc<-c(seq(88517,nrow(Couples_investigate),1000))

Lag_time<-3


## Choose matrix for POT

Couples_investigate<-Allcomb

              ########## CHOOSE TYPE OF ANALYSIS ###############

#TYPE_ANALYSIS<-"Max Annual"
#TYPE_ANALYSIS<-"Variable POT Month"
TYPE_ANALYSIS<- "POT mixed approach"


############################# Initialize POT matrix ##################################


POT_matrix<-as.data.frame(matrix(, nrow = nrow(Couples_investigate), ncol = 18))

colnames(POT_matrix)<-c("CODE", "ID_Station_1",  "ID_Station_2",
                        "Easting_1","Northing_1","Easting_2","Northing_2","Distance","Year_st","Year_end"
                        , "POT_KT_12", "POT_pvalue_12", "Syn_12",  "POT_KT_21", "POT_pvalue_21","Syn_21",
                        "MAX_ANNUAL_KT", "MAX_ANNUAL_pvalue")

# 
# colnames(POT_matrix)<-c("CODE", "ID_Station_1",  "ID_Station_2","KendalT.value","KendalT.p.value","Number_data",
#                         "Num_Syncr_occ","Num_Asyncr_occ",
#                         "Easting_1","Northing_1","Easting_2","Northing_2","Distance","Year_st","Year_end"
#                         , "POT_KT_12", "POT_pvalue_12",   "POT_KT_21",       "POT_pvalue_21")
# 
# colnames(KT_matrix)=c("River_St_1","River_St_2","KendalT.value", "KendalT.p.value",
#                       "N_ties Station1","N_ties Station2","Number_data", "R_Ties_sum","ID_Station_1", 
#                       "ID_Station_2","Num_Syncr_occ","Num_Asyncr_occ","KT_ Asyncrony", "KT_pvalue_Asy",
#                       "KT_ Syncrony", "KT_pvalue_Sy","Easting_1","Northing_1","Easting_2","Northing_2")                  # Kendall tau matrix
# 


#############################1. CYCLE FOR ALL THE COUPLES ######################################


List_couple<- vector(mode = "list", length = nrow(Couples_investigate))
#List_couple_R<- vector(mode = "list", length = nrow(Couples_investigate))
List_pettiT<- vector(mode = "list", length = nrow(Couples_investigate))

#Threshold<-list()
#Th_num<-list()

xx<-1

for (yy in 1:nrow(Couples_investigate))
{
  
  
    ######
    ID_Station_1<-Couples_investigate$ID_Station_1[yy]
    ID_Station_2<-Couples_investigate$ID_Station_2[yy]
  
    if(yy%in%comb_perc)
    {print(paste("The code is running: couple",yy,"of",nrow(Couples_investigate)," - Percentage:", round(100*yy/nrow(Couples_investigate),2),"%"))}
    
    ## IMPORT POT VALUES FROM POT DATASET
    
    setwd(paste0(path,"/Data/POT_data/Suitable"))
    
    #Station1
    name1<- ifelse(nchar(ID_Station_1)==4, paste0("00",ID_Station_1),if(nchar(ID_Station_1)==5){paste0("0",ID_Station_1)}else{ID_Station_1})
    #Station2
    name2<- ifelse(nchar(ID_Station_2)==4, paste0("00",ID_Station_2),if(nchar(ID_Station_2)==5){paste0("0",ID_Station_2)}else{ID_Station_2})
    
    if(any(paste0(name1,".pt")==File_st) && any(paste0(name2,".pt")==File_st))
    { 
      
    #Station1: read file preparing
    con1 <- file(paste0(name1,".pt"),"r") 
    tt<-readLines(con1,n=1000)
    num_st1<-which(tt=="[POT Values]")
    tnm1<-tt[6]
    close(con1)   
    
    
    #Station2: read file preparing
    name2<- ifelse(nchar(ID_Station_2)==4, paste0("00",ID_Station_2),if(nchar(ID_Station_2)==5){paste0("0",ID_Station_2)}else{ID_Station_2})
    
    con2 <- file(paste0(name2,".pt"),"r") 
    tt<-readLines(con2,n=1000)
    num_st2<-which(tt=="[POT Values]")
    tnm2<-tt[6]
    close(con2)   
    

    # Threshold[[xx]]<- list(tnm1,tnm2)
    # Th_num[[xx]]<-list(as.numeric(as.character(str_split(tnm1,"\\,")[[1]][2])),as.numeric(as.character(str_split(tnm2,"\\,")[[1]][2])))
   
    # names(Threshold)[[xx]]<-Couples_investigate$CODE[yy]
    # names(Th_num)[[xx]]<-Couples_investigate$CODE[yy]
    
    
    #Part 1: 
    # Read Discharge data and extract just data for year in common
    
    #Read daily discharge for Station 1
    
    Station_1<- read.csv(paste0(name1,".pt"),sep=",", skip=num_st1,header=FALSE)
    colnames(Station_1)<-c("Data","Discharge_1","bas")
    
    #Read daily discharge for Station 2
    
    Station_2<- read.csv(paste0(name2,".pt"),skip=num_st2,sep=",", header=FALSE)
    colnames(Station_2)<-c("Data","Discharge_2","bas")
    
    
    Station_1$Data_1<-as.Date(as.POSIXct(Station_1$Data, format="%d %b %Y",tz="UTC"))
    Station_2$Data_2<-as.Date(as.POSIXct(Station_2$Data, format="%d %b %Y",tz="UTC"))
    
    
    Station_1$Year<-year(as.Date(Station_1$Data_1, format="%d %b %Y"))
    Station_2$Year<-year(as.Date(Station_2$Data_2, '%Y-%m-%d'))
    
    Station_1$Month<-month(as.Date(Station_1$Data_1, '%Y-%m-%d'))
    Station_2$Month<-month(as.Date(Station_2$Data_2, '%Y-%m-%d'))
    
    X1<-data.frame(table(Station_1$Year))
    X2<-data.frame(table(Station_2$Year))
    
    #Year in common between the two couples
    
    Year_final<- merge(X1,X2, by.x = "Var1", by.y = "Var1")
    Final_merged<-na.omit(merge(Station_1,Station_2, by.x = "Year", by.y = "Year"))
    
    D1<-which(names(Daily_data)==ID_Station_1)
    D2<-which(names(Daily_data)==ID_Station_2)
    
    ## IMPORT DAILY DISCHARGE
  if(length(D1)>0 && length(D2)>0)
  {
  if(unique(Daily_data[D1]!="No data available") && unique(Daily_data[D2]!="No data available"))
  {
    DAILY_Station_1<- cbind(rownames(data.frame(Daily_data[[D1]])),data.frame(Daily_data[[D1]]))
    colnames(DAILY_Station_1)<-c("Data_1","Discharge_1")
    
    #Read daily discharge for Station 2
    
    DAILY_Station_2<- cbind(rownames(data.frame(Daily_data[[D2]])),data.frame(Daily_data[[D2]]))
    colnames(DAILY_Station_2)<-c("Data_2","Discharge_2")
    
    
    DAILY_Station_1$Data_1<-as.Date(as.POSIXct(DAILY_Station_1$Data_1, format="%Y-%m-%d",tz="UTC"))
    DAILY_Station_2$Data_2<-as.Date(as.POSIXct(DAILY_Station_2$Data_2, format="%Y-%m-%d",tz="UTC"))
    
    
    DAILY_Station_1$Year<-year(as.Date(DAILY_Station_1$Data_1, '%Y-%m-%d'))
    DAILY_Station_2$Year<-year(as.Date(DAILY_Station_2$Data_2, '%Y-%m-%d'))
    
    DAILY_Station_1$Month<-month(as.Date(DAILY_Station_1$Data_1, '%Y-%m-%d'))
    DAILY_Station_2$Month<-month(as.Date(DAILY_Station_2$Data_2, '%Y-%m-%d'))
    
    DAILY_Station_1$Juliand_1<-julian.Date(DAILY_Station_1$Data_1,origin=as.Date("1940-01-01"))
    DAILY_Station_2$Juliand_2<-julian.Date(DAILY_Station_2$Data_2,origin=as.Date("1940-01-01"))
    
    #Year in common between the two couples
    
    DAILY_X1<-data.frame(table(DAILY_Station_1$Year))
    DAILY_X2<-data.frame(table(DAILY_Station_2$Year))
    
    
    DAILY_Year_final<- merge(DAILY_X1,DAILY_X2, by.x = "Var1", by.y = "Var1")
    
    FINAL_y<-which(Year_final$Var1%in%DAILY_Year_final$Var1)
    
    Final_Multi_merged<-na.omit(merge(Year_final,DAILY_Year_final, by.x = "Var1", by.y = "Var1"))
    
    if(length(unique( Final_Multi_merged$Var1))>5){
    
      Y_st<- as.numeric(as.character( Final_Multi_merged$Var1[1]))
      Y_end<-as.numeric(as.character( Final_Multi_merged$Var1[nrow( Final_Multi_merged)]))
      
      #Y_st<- Final_merged$Data_1[1]
      #Y_end<-Final_merged$Data_1[nrow(Final_merged)]
      
      ##################### DISCHARGE FOR COMMON YEAR ##########################
      
      Stat_1_adj<-Station_1[which(Station_1$Year==Y_st)[1]:last(which(Station_1$Year==Y_end)),]
      Stat_2_adj<-Station_2[which(Station_2$Year==Y_st)[1]:last(which(Station_2$Year==Y_end)),]
      
      #Julian Day
      Stat_1_adj$Juliand_1<-julian.Date(Stat_1_adj$Data_1,origin=as.Date("1940-01-01"))
      Stat_2_adj$Juliand_2<-julian.Date(Stat_2_adj$Data_2,origin=as.Date("1940-01-01"))
      
      
      ##########################################################################
      ##########  Calculate multivariate POT and daily discharge  ##############
      
      #Lag_time<-3
        
      Dep_12<-POT_eventmix_12(Stat_1_adj,Stat_2_adj,DAILY_Station_2,Lag_time)
      Dep_21<-POT_eventmix_21(Stat_2_adj,Stat_1_adj,DAILY_Station_1,Lag_time)    
      
      ###################### MAximum annual with new dataset ###################
      
      Dep_A<-POT_annualmax(Stat_1_adj,Stat_2_adj)
  

      ################## Randomization and KT ##############################################
      
      source(paste0(path,"/Code/Functions/","Randomization.R"))
      
      
      Dep_12_Discharge<-Dep_12[,c("Discharge_1","Discharge_12")]
      Dep_21_Discharge<-Dep_21[,c("Discharge_2","Discharge_21")]
      Dep_A_Discharge<-Dep_A[,c("QPeak_1","QPeak_2")]
      
      
      Dep_12_Disc_R<-Randomization(Dep_12_Discharge,0.1)
      Dep_21_Disc_R<-Randomization(Dep_21_Discharge,0.1)
      Dep_A_Disc_R<-Randomization(Dep_A_Discharge,0.1)      
      
      ############ KT for at least 20 data ################################################
      
      if(nrow(Dep_12_Disc_R)>=20)
        
      {KT.test_12 <- cor.test(Dep_12_Disc_R[,1],Dep_12_Disc_R[,2],method="kendall") }
      
      if(nrow(Dep_21_Disc_R)>=20)
        
      {KT.test_21 <- cor.test(Dep_21_Disc_R[,1],Dep_21_Disc_R[,2],method="kendall") }

      if(nrow(Dep_A)>=20)
        
      {
       KT.test_A <- cor.test(Dep_A_Disc_R[,1],Dep_A_Disc_R[,2],method="kendall") 
       POT_matrix$MAX_ANNUAL_KT[xx]<- KT.test_A$estimate
       POT_matrix$MAX_ANNUAL_pvalue[xx]<- KT.test_A$p.value
       
       List_pettiT[[xx]]<-list(pettitt.test(Dep_A_Disc_R[,1]),pettitt.test(Dep_A_Disc_R[,2]))
       names(List_pettiT)[xx]<-yy
       names(List_pettiT[[xx]])<-c(ID_Station_1,ID_Station_2)

       }      
      
      POT_matrix$POT_KT_12[xx]<- KT.test_12$estimate
      POT_matrix$POT_pvalue_12[xx]<- KT.test_12$p.value 
      POT_matrix$Syn_12[xx]<-length(which(Dep_12$PEAK==TRUE))/nrow(Dep_12)
      
      
      POT_matrix$POT_KT_21[xx]<- KT.test_21$estimate
      POT_matrix$POT_pvalue_21[xx]<- KT.test_21$p.value
      POT_matrix$Syn_21[xx]<-length(which(Dep_21$PEAK==TRUE))/nrow(Dep_21)      
     

      POT_matrix$CODE[xx]<-yy
      POT_matrix$ID_Station_1[xx]<-ID_Station_1
      POT_matrix$ID_Station_2[xx]<-ID_Station_2
      
      
      cc_l1<- which(M_coord$Station.number==ID_Station_1)
      cc_l2<- which(M_coord$Station.number==ID_Station_2)
      
      POT_matrix$Easting_1[xx]<-M_coord$Easting[cc_l1]
      POT_matrix$Northing_1[xx]<-M_coord$Northing[cc_l1]
      
      POT_matrix$Easting_2[xx]<-M_coord$Easting[cc_l2]
      POT_matrix$Northing_2[xx]<-M_coord$Northing[cc_l2]       
      
      POT_matrix$Distance[xx]<-distGeo(c(M_coord$Longitude[cc_l1], M_coord$Latitude[cc_l1]), c(M_coord$Longitude[cc_l2], M_coord$Latitude[cc_l2]))
      
      POT_matrix$Year_st[xx]<-Y_st
      POT_matrix$Year_end[xx]<-Y_end
    
      
      List_couple[[xx]]<-list(Dep_12,Dep_21, Dep_A)
      names(List_couple)[xx]<-yy
      names(List_couple[[xx]])<-c(ID_Station_1,ID_Station_2, "Annual Max")
      
      # List_couple_R[[xx]]<-Dep_12_Disc_R
      # names(List_couple_R)[xx]<-yy
      # 

      
      xx<-xx+1

    }}}
}}


print(paste0("You run the analysis:",TYPE_ANALYSIS))

na_pos<-which(is.na(POT_matrix$CODE))


POT_matrix_without_na<-POT_matrix2[-na_pos,]


############ Saving results #################


   setwd("C:/PROJECTS 2021/QQ_POT_mixed/Results")
  
  save.image(paste0("POT_Mix2_lag",Lag_time,".RData"))
  save(POT_matrix,file=paste0("Final_matrix_POTMix2_lag",Lag_time,".RData"))

  write.table(POT_matrix,paste0("POT_Mix2_lag",Lag_time,".csv"), row.names=F, col.names=T)

############################################## 

