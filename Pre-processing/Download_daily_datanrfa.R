
## Download from Nrfa dataset the daily data ###

path<-c("C:/PROJECTS 2021/QQ_POT_mixed/Data")

daily_available<-read.csv("C:/PROJECTS 2021/QQ_POT_mixed/Data/daily_data_available.csv", sep=";")


source("C:/PROJECTS 2021/QQ_POT_mixed/Functions/Daily_data_nrfa.R")

Daily_data<-Daily_data_nrfa(daily_available)

save(Daily_data, file="C:/PROJECTS 2021/QQ_POT_mixed/Data/Daily_data.RData")

#########################################################################
