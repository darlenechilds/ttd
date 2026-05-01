# darlene.childs@dfo-mpo.gc.ca
# April 2026
# Timeseries plot for do for different water masses using ocads data

rm(list = ls())
library(oce)
library(dplyr)

#compiled from 00_readinOCADS_tracer_o2 script
d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv", stringsAsFactors = F)
# d <- read.csv("data/processed/UNSUB_tracers_o2.csv")  


head(d)

#get do dataframe
d <- d%>%
  select("EXPOCODE", "STNNBR", "BTLNBR","SAMPNO", "DATE",  "LATITUDE", "LONGITUDE",
         "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"OXYGEN","OXYGEN_FLAG_W","year")
unique(d$year)

#filter/keep good data usign ocads quality control flags, 2 = good, 6 = median of replicates
d <- d[d$OXYGEN_FLAG_W==2 | d$OXYGEN_FLAG_W==6 | d$OXYGEN_FLAG_W==0,]
d <- d[rowSums(is.na(d)) != ncol(d), ]

#remove do = 0, -999
d$OXYGEN[d$OXYGEN == -999] <- NA
d <- d[!is.na(d$OXYGEN), ]

# plot(d$OXYGEN,-d$CTDPRS)

# 2022 do data in ml/l, only one decimal place reported
# get 2022 data
d1 <- d[d$year==2022,]
#convert DO ml/l to umol/kg (1 kg/m^3 = 0.001 kg/L) 
theta <- swTheta(d1$CTDSAL, d1$CTDTMP, d1$CTDPRS, referencePressure = 0)
dens <- (swRho(d1$CTDSAL,theta,d1$CTDPRS))/1000  # kg/L
conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volume, 1 mole of O2 = 22.4 L, 1/22.3916*10^6)
d1$OXYGEN <- d1$OXYGEN*conv/dens
# remove 2022
d <- d[!d$year==2022,]
# add 2022 back in
d <- rbind.data.frame(d,d1)
# check
# plot(d$OXYGEN,-d$CTDPRS)

# further cleaning
ind <- which(d$OXYGEN < 265) 
d <- d[-ind,]
d2 <- d[d$CTDPRS>500,]
plot(d2$OXYGEN,-d2$CTDPRS)
ind2 <- which(d2$OXYGEN>312)
d2 <- d2[-ind2,]
#remove bottom
d <- d[!d$CTDPRS>500,]
#put clean bottom back in
d <- rbind.data.frame(d,d2)

# plot(d$OXYGEN,-d$CTDPRS)


# lookign at years... 
d1 <- d[d$year==2020,]
d2 <- d[d$year==2024,]
d3 <- d[d$year==2023,]

plot(d1$OXYGEN,-d1$CTDPRS)
points(d2$OXYGEN,-d2$CTDPRS, col = "blue")
points(d3$OXYGEN,-d3$CTDPRS, col = "red")

deep <- d1[d1$CTDPRS>1900,]

unique(d$year)

#calculate %sat and AOU


#find NEADW
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.015),]

neadw_ave_ocads <- mean(neadw$OXYGEN)
neadw_sd_ocads <- sd(neadw$OXYGEN)

uyear <- unique(d$year)
s_ne = NULL

for (yr in uyear){
  
  e <- neadw[neadw$year == yr, ]
  
  mean <- mean(e$OXYGEN, na.rm = TRUE)
  sd  <- sd(e$OXYGEN, na.rm = TRUE)
  anomaly  <- mean - neadw_ave_ocads
  n <- length(e$EXPOCODE)
  e_df <- data.frame(year = yr,
                     mean = mean,
                     sd = sd,
                     anomaly = anomaly,
                     n = n)
  
  s_ne <- rbind(s_ne, e_df)
}
s_ne <- s_ne[order(s_ne$year), ]

#find NvLSW

nvlsw <- d[d$LATITUDE > 56 & d$LATITUDE <60,]
nvlsw <- nvlsw[nvlsw$CTDPRS > 150 & nvlsw$CTDPRS < 500,]

nvlsw_ave_ocads <- mean(nvlsw$OXYGEN)
nvlsw_sd_ocads <- sd(nvlsw$OXYGEN)

uyear <- unique(d$year)
s_nv = NULL

for (yr in uyear){
  
  e <- nvlsw[nvlsw$year == yr, ]
  
  mean <- mean(e$OXYGEN, na.rm = TRUE)
  sd  <- sd(e$OXYGEN, na.rm = TRUE)
  anomaly  <- mean - nvlsw_ave_ocads
  n <- length(e$EXPOCODE)
  e_df <- data.frame(year = yr,
                     mean = mean,
                     sd = sd,
                     anomaly = anomaly,
                     n = n)
  
  s_nv <- rbind(s_nv, e_df)
}
s_nv <- s_nv[order(s_nv$year), ]

###  plot on one panel 
half_sd <- s_nv$sd/2
ulim <- s_nv$mean+half_sd
llim <- s_nv$mean-half_sd

plot(s_nv$year,s_nv$mean, type = "both", pch = 19, ylim = c(260,320), xlim = c(1990, 2026), 
     main = "Dissolved Oxygen Timeseries", ylab = "[DO] (umol kg-1)", xlab = "year")
arrows(s_nv$year,llim, s_nv$year,ulim, angle = 90, code = 3, length = 0.05)
lines(c(1992, 2025), c(nvlsw_ave_ocads, nvlsw_ave_ocads), col = "red", lwd = 1,lty = 2)

points(s_ne$year,s_ne$mean, type = "both", pch = 19,col = "blue")
half_sd <- s_ne$sd/2
ulim <- s_ne$mean+half_sd
llim <- s_ne$mean-half_sd
arrows(s_ne$year,llim, s_ne$year,ulim, angle = 90, code = 3, length = 0.05, col = "blue")
lines(c(1992, 2025), c(neadw_ave_ocads, neadw_ave_ocads), col = "red", lwd = 1,lty = 2)

legend("topright",
       legend = c("NvLSW - 150<depth<500m, 56<Lat<59.1", 
                  "NEADW - 36.96< σ2<37.1, 56<Lat<60", 
                  "Climatology - mean 1992:2025"),
       pch = c(19),
       col = c("black", "blue", "red"),
       bty = "n")
