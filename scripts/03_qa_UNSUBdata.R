# Flags from lab data for tracers have already been removed, but need to look at DO.. 
rm(list = ls())
library(dplyr)
library(oce)

#load data
d <- read.csv("data/UNSUB_tracers_o2.csv")   #read in tracer data

o2_flag <- which(d$OXYGEN_umolperkg<220)

d <- d[-o2_flag,]


ucruise <- unique(d$EXPOCODE)
i <- ucruise[2]
j <- ustn[1]
for (i in ucruise){
  e <- d[which(d$EXPOCODE==i),]
  ustn <- as.numeric(unique(e$STNNBR))
    for (j in ustn){
      f <- e[which(e$STNNBR==j),]
      
      par(mfrow = c(1,4), mar = c(5, 2, 3, 1 ))
      plot(f$SF6,-f$CTDPRS,pch = 16, xlim = c(0,5), ylim = c(-3700,0), main = paste(i,j))
      plot(f$OXYGEN_umolperkg,-f$CTDPRS,pch = 16, xlim = c(0,500),ylim = c(-3700,0))
      plot(f$CTDTMP, -f$CTDPRS,pch = 16, xlim = c(-1.5,5),ylim = c(-3700,0))
      plot(f$CTDSAL, -f$CTDPRS,pch = 16, xlim = c(34.7, 35),ylim = c(-3700,0))
    }
}
