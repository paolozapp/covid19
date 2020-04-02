#Source: https://www.istat.it/it/archivio/240401

library(tidyverse)
library(dplyr)

#Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set working directory to this folder, RStudio
setwd(getSrcDirectory()[1]) #set working directory to this folder, R
rm(list=ls()) #delete all variables

#Upload data
deaths <- read.table(file = 'comune_giorno.csv',header=TRUE,sep=",",fill=TRUE)

#Lombardy
lombardy <- deaths %>% filter(NOME_REGIONE == "Lombardia")
#for the moment we are not considering gender
data <- lombardy[,c(4,7,8,22,23,24,25,26)]
#some data are not available
#MEMO: extend results to the full dataset
data <- data %>% filter(TOTALE_20 != 9999)
data[,4] <- as.numeric(as.character(data[,4]))
data[,5] <- as.numeric(as.character(data[,5]))
data[,6] <- as.numeric(as.character(data[,6]))
data[,7] <- as.numeric(as.character(data[,7]))
data[,8] <- as.numeric(as.character(data[,8]))
#gathered data by province
provinces <- data %>% group_by(NOME_PROVINCIA,CL_ETA,GE) %>% summarise(TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))
total <- data %>% group_by(GE) %>% summarise(TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))
plot(total$GE,total$TOTALE_16,type="p",col="black",pch=16,ylim=c(0,500))
points(total$GE,total$TOTALE_17,col="black",pch=16)
points(total$GE,total$TOTALE_18,col="black",pch=16)
points(total$GE,total$TOTALE_19,col="black",pch=16)
lines(total$GE,total$TOTALE_20,col="red")
 