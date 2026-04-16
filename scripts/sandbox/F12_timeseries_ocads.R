#timeseries plot for f12for different water masses using ocads data

rm(list = ls())
library(oce)

#compiled from 00_readinOCADS_tracer_o2 script
d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv", stringsAsFactors = F)
head(d)
#get F12 dataframe
d <- d%>%
  select("EXPOCODE", "STNNBR", "BTLNBR","SAMPNO", "DATE",  "LATITUDE", "LONGITUDE",
         "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"cfc12","cfc12_flag","year")
unique(d$year)
#filter/keep good data usign ocads quality control flags, 2 = good, 6 = median of replicates
# d <- d[d$cfc12_flag==2 | d$cfc12_flag==6 | d$cfc12_flag==0,]  #includes 2023 
d <- d[d$cfc12_flag==2 | d$cfc12_flag==6 ,]  #excluded 2023  CFC12 data not reliable

# lookign at years... 
d1 <- d[d$year==2023,]
unique(d1$cfc12_flag)
d2 <- d[d$year==2024,]
d3 <- d[d$year==2022,]

plot(d1$cfc12,-d1$CTDPRS)
points(d2$cfc12,-d2$CTDPRS, col = "blue")
points(d3$cfc12,-d3$CTDPRS, col = "red")

#check
plot(d$LONGITUDE,d$LATITUDE)
plot(d$cfc12,-d$CTDPRS)

#remove f12 = 0
d <- d[d$cfc12 != 0, ]

unique(d$year)

#find NEADW
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$CTDTMP,d$CTDPRS)

neadw <- d[which(d$LATITUDE > 56 & d$LONGITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.04),]

neadw_avef12_ocads <- mean(neadw$cfc12)
neadw_sdf12_ocads <- sd(neadw$cfc12)

uyear <- unique(d$year)
s_ne = NULL

for (yr in uyear){
  
  e <- neadw[neadw$year == yr, ]
  
  f12ave <- mean(e$cfc12, na.rm = TRUE)
  f12sd  <- sd(e$cfc12, na.rm = TRUE)
  f12an  <- f12ave - neadw_avef12_ocads
  n <- length(e$EXPOCODE)
  e_df <- data.frame(year = yr,
                     mean = f12ave,
                     sd = f12sd,
                     anomaly = f12an,
                     n = n)
  
  s_ne <- rbind(s_ne, e_df)
}

s_ne <- s_ne[order(s_ne$year), ]

half_sd <- s_ne$sd/2
ulim <- s_ne$mean+half_sd
llim <- s_ne$mean-half_sd

plot(s_ne$year,s_ne$mean, type = "both", pch = 19, ylim = c(0.5,3), xlim = c(1990, 2026), 
     main = "NEADW timeseries", ylab = "[f12] (pmol kg-1)", xlab = "year")
arrows(s_ne$year,llim, s_ne$year,ulim, angle = 90, code = 3, length = 0.05)


#find NvLSW

nvlsw <- d[d$LATITUDE > 56 & d$LONGITUDE <60,]
nvlsw <- nvlsw[nvlsw$CTDPRS > 150 & nvlsw$CTDPRS < 500,]

nvlsw_avef12_ocads <- mean(nvlsw$cfc12)
nvlsw_sdf12_ocads <- sd(nvlsw$cfc12)

uyear <- unique(d$year)
s_nv = NULL

for (yr in uyear){
  
  e <- nvlsw[nvlsw$year == yr, ]
  
  f12ave <- mean(e$cfc12, na.rm = TRUE)
  f12sd  <- sd(e$cfc12, na.rm = TRUE)
  f12an  <- f12ave - neadw_avef12_ocads
  n <- length(e$EXPOCODE)
  e_df <- data.frame(year = yr,
                     mean = f12ave,
                     sd = f12sd,
                     anomaly = f12an,
                     n = n)
  
  s_nv <- rbind(s_nv, e_df)
}
s_nv <- s_nv[order(s_nv$year), ]
half_sd <- s_nv$sd/2
ulim <- s_nv$mean+half_sd
llim <- s_nv$mean-half_sd

plot(s_nv$year,s_nv$mean, type = "both", pch = 19, ylim = c(0.5,4), xlim = c(1990, 2026), 
     main = "NvLSW timeseries", ylab = "[f12] (pmol kg-1)", xlab = "year")
arrows(s_nv$year,llim, s_nv$year,ulim, angle = 90, code = 3, length = 0.05)
