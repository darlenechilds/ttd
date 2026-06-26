# plot TS for centre lab sea from 2015-2025
rm(list = ls())
library(oce)

d <- read.csv("data/UNSUB_tracers_o2.csv")    #UNSUBMITTED data, data from local instrument
d$year <- as.integer(substr(d$EXPOCODE, 4, 7))
d$year[d$EXPOCODE == "AT4805"] <- 2022  # example only
d$year[d$EXPOCODE == " AMU2019001"] <- 2019  # example only


d <- d[d$LATITUDE>56 & d$LATITUDE <59.1,]          
# d <- d[d$CTDPRS>150 & d$CTDPRS<800,]

d <- d[d$year<2024,]
d2023 <- d[d$year==2023,]
plot(d$CTDSAL,d$CTDTMP, pch = 19)
points(d2023$CTDSAL,d2023$CTDTMP, pch = 19, col = "blue")



