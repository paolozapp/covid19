#Source: https://www.istat.it/it/archivio/240401

library(tidyverse)
library(dplyr)

#Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set working directory to this folder, RStudio
setwd(getSrcDirectory()[1]) #set working directory to this folder, R
rm(list=ls()) #delete all variables

#Upload data
deaths2020 <- read.table(file = 'comune_giorno.csv',header=TRUE,sep=",",fill=TRUE)
head(deaths2020)

#gathered data by province, for the moment we are not considering gender
data <- deaths2020[,c(3,4,7,8,22,23,24,25,26)]
data[,5] <- as.numeric(as.character(data[,5]))
data[,6] <- as.numeric(as.character(data[,5]))
data[,7] <- as.numeric(as.character(data[,5]))
data[,8] <- as.numeric(as.character(data[,5]))
data[,9] <- as.numeric(as.character(data[,5]))

data <- data %>% group_by(NOME_REGIONE,NOME_PROVINCIA,CL_ETA,GE) %>% summarise(TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))
data <- data[5:115252,]

# bisogna sistemare 29/02 e GE in modo che sia progressivo

#gathered for all Italy
italy <- data %>% group_by (GE) %>% summarise(TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))
italy <- italy[2:172,]
plot(italy$GE,italy$TOTALE_17)
 