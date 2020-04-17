#Source: https://www.istat.it/it/archivio/240401

#libraries
install.packages("tidyverse")
install.packages("dplyr")

library(tidyverse)
library(dplyr)

#Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set working directory to this folder, RStudio
setwd(getSrcDirectory()[1]) #set working directory to this folder, R
rm(list=ls()) #delete all variables

#Upload data
ISTAT_dataset <- read.table(file = 'comune_giorno.csv',header=TRUE,sep=",",fill=TRUE)
days <- c("0101","0102","0103","0104","0105","0106","0107","0108","0109","0110",
          "0111","0112","0113","0114","0115","0116","0117","0118","0119","0120",
          "0121","0122","0123","0124","0125","0126","0127","0128","0129","0130","0131",
          "0201","0202","0203","0204","0205","0206","0207","0208","0209","0210",
          "0211","0212","0213","0214","0215","0216","0217","0218","0219","0220",
          "0221","0222","0223","0224","0225","0226","0227","0228",
          "0301","0302","0303","0304","0305","0306","0307","0308","0309","0310",
          "0311","0312","0313","0314","0315","0316","0317","0318","0319","0320",
          "0321","0322","0323","0324","0325","0326","0327","0328","0329","0330","0331",
          "0401","0402","0403","0404","0405","0406","0407","0408","0409","0410",
          "0411","0412","0413","0414","0415","0416","0417")#,"0418","0419","0420",
          #"0421","0422","0423","0424","0425","0426","0427","0428","0429","0430")
aftermarch <- c("0301","0302","0303","0304","0305","0306","0307","0308","0309","0310",
      "0311","0312","0313","0314","0315","0316","0317","0318","0319","0320",
      "0321","0322","0323","0324","0325","0326","0327","0328","0329","0330","0331",
      "0401","0402","0403","0404","0405","0406","0407","0408","0409","0410",
      "0411","0412","0413","0414","0415","0416","0417")#,"0418","0419","0420",
#"0421","0422","0423","0424","0425","0426","0427","0428","0429","0430")

#only days of interest
ISTAT_italy <- ISTAT_dataset %>% filter(GE %in% days)

#let us gather data: (Region,Province,Town,Day,Total_death2015,2016,2017,2018,2019,2020)

ISTAT_italy <- ISTAT_italy[,c("NOME_REGIONE","NOME_PROVINCIA","NOME_COMUNE","GE","TOTALE_15","TOTALE_16","TOTALE_17","TOTALE_18","TOTALE_19","TOTALE_20")]

#adding a flag "missing = TRUE" if the data from 2020 is missing and giving value 0 instead of 9999
ISTAT_italy <- cbind(ISTAT_italy, missing = ISTAT_italy$TOTALE_20 == 9999)
ISTAT_italy[which(ISTAT_italy$TOTALE_20 == 9999),"TOTALE_20"] <- 0

#from factor to numeric
ISTAT_italy[,"TOTALE_15"] <- as.numeric(as.character(ISTAT_italy[,"TOTALE_15"]))
ISTAT_italy[,"TOTALE_16"] <- as.numeric(as.character(ISTAT_italy[,"TOTALE_16"]))
ISTAT_italy[,"TOTALE_17"] <- as.numeric(as.character(ISTAT_italy[,"TOTALE_17"]))
ISTAT_italy[,"TOTALE_18"] <- as.numeric(as.character(ISTAT_italy[,"TOTALE_18"]))
ISTAT_italy[,"TOTALE_19"] <- as.numeric(as.character(ISTAT_italy[,"TOTALE_19"]))
ISTAT_italy[,"TOTALE_20"] <- as.numeric(as.character(ISTAT_italy[,"TOTALE_20"]))

#summing data from all the ages
data_italy <- ISTAT_italy %>% group_by(NOME_REGIONE,NOME_PROVINCIA,NOME_COMUNE,GE,missing) %>% summarise(TOTALE_15 = sum(TOTALE_15),TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))
