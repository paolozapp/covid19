#Source: https://www.istat.it/it/archivio/240401

library(tidyverse)
library(dplyr)

#Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set working directory to this folder, RStudio
setwd(getSrcDirectory()[1]) #set working directory to this folder, R
rm(list=ls()) #delete all variables

#Upload data
deaths <- read.table(file = 'comune_giorno.csv',header=TRUE,sep=",",fill=TRUE)
gooddays <- c("0101","0102","0103","0104","0105","0106","0107","0108","0109","0110",
          "0111","0112","0113","0114","0115","0116","0117","0118","0119","0120",
          "0121","0122","0123","0124","0125","0126","0127","0128","0129","0130","0131",
          "0201","0202","0203","0204","0205","0206","0207","0208","0209","0210",
          "0211","0212","0213","0214","0215","0216","0217","0218","0219","0220",
          "0221","0222","0223","0224","0225","0226","0227","0228",
          "0301","0302","0303","0304","0305","0306","0307","0308","0309","0310",
          "0311","0312","0313","0314","0315","0316","0317","0318","0319","0320",
          "0321","0322","0323","0324","0325","0326","0327","0328","0329","0330","0331")

#Lombardy
lombardy <- deaths %>% filter(NOME_REGIONE == "Lombardia") %>% filter(GE %in% gooddays)
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
total$GE <- droplevels(total$GE)
plot(2,3,type="n",xlim=c(1,91),ylim=c(0,400),xlab="Day of the year",ylab = "Daily deaths",xaxt='n')
axis(1,at=c(1,32,60,91),labels=c("Jan 1","Feb 1","Mar 1","Apr 1"))
title(main = "Lombardy daily deaths")
points(total$GE,total$TOTALE_16,col="gray",pch=20)
points(total$GE,total$TOTALE_17,col="gray",pch=20)
points(total$GE,total$TOTALE_18,col="gray",pch=20)
points(total$GE,total$TOTALE_19,col="gray",pch=20)
lines(total$GE,total$TOTALE_20,col="red",lwd=2)
abline(v=c(32,60,91),lty=3)
 