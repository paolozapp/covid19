#Source: https://www.istat.it/it/archivio/240401

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); #set working directory to this folder, RStudio
setwd(getSrcDirectory()[1]) #set working directory to this folder, R

rm(list=ls()) #delete all variables

deaths2020 <- read.table(file = 'comune_giorno.csv',header=TRUE,sep=",")
head(deaths2020)
