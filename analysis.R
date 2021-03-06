# run clean_dataset.R: you need values 'days','aftermarch','officialtolldays' and data 'data_italy'

# considering only known data in 2020
known <- data_italy %>% filter (missing == FALSE)

# summing up all data from the same day
daybyday_known <- known %>% group_by (GE) %>% summarise(TOTALE_15 = sum(TOTALE_15), TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))

# dimension of our sample
available <- c(sum(known$TOTALE_15)/sum(data_italy$TOTALE_15),
              sum(known$TOTALE_16)/sum(data_italy$TOTALE_16),
              sum(known$TOTALE_17)/sum(data_italy$TOTALE_17),
              sum(known$TOTALE_18)/sum(data_italy$TOTALE_18),
              sum(known$TOTALE_19)/sum(data_italy$TOTALE_19))
cat("Only",round(mean(available)*100,1),"% of the dataset is available")

# Daily deaths (sample)
plot(0,0,type="n",xlim=c(1,dim(daybyday_known)[1]),ylim=c(0,round(max(daybyday_known$TOTALE_20)/100)+1)*100,xlab="Day of the year",ylab = "Daily deaths",xaxt='n')
axis(1,at=c(1,32,60,91),labels=c("Jan 1","Feb 1","Mar 1","Apr 1"))
title(main = "Daily deaths, Italy (sample)")
points(daybyday_known$GE,daybyday_known$TOTALE_15,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_16,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_17,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_18,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_19,col="gray",pch=20)
lines(daybyday_known$GE,daybyday_known$TOTALE_20,col="red",lwd=2)
abline(v=c(1,32,60,91),lty=3)

#superposition of dailytoll to average deaths
expectedtoll <- cbind(day = days[-which(days %in% officialtolldays)],dailytoll = rep(0,length(days[-which(days %in% officialtolldays)])))
expectedtoll <- rbind(expectedtoll,toll_italy)
#adding average death 2015-2019
expectedtoll <- expectedtoll[1:dim(daybyday_known)[1],]
expectedtoll[,"dailytoll"] <- as.numeric(expectedtoll[,"dailytoll"]) + rowSums(daybyday_known[,2:6])/5

plot(0,0,type="n",xlim=c(1,dim(daybyday_known)[1]),ylim=c(0,round(max(as.numeric(expectedtoll[,2]))/100)+1)*100,xlab="Day of the year",ylab = "Daily deaths",xaxt='n')
axis(1,at=c(1,32,60,91),labels=c("Jan 1","Feb 1","Mar 1","Apr 1"))
title(main = "Daily deaths, Italy")
points(daybyday_known$GE,daybyday_known$TOTALE_15,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_16,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_17,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_18,col="gray",pch=20)
points(daybyday_known$GE,daybyday_known$TOTALE_19,col="gray",pch=20)
lines(daybyday_known$GE,daybyday_known$TOTALE_20,col="red",lwd=2)
lines(daybyday_known$GE,expectedtoll[,2],col="blue",lwd=2)
abline(v=c(1,32,60,91),lty=3)
legend("topleft",legend=c("Daily deaths (sample)","Official toll + avg"),col=c("red","blue"),lty=c(1,1), cex=0.8)
