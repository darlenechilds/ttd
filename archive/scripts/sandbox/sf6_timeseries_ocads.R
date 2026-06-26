#timeseries plot for sf6 for different water masses using ocads data

rm(list = ls())
library(oce)

#compiled from 00_readinOCADS_tracer_o2 script
d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv", stringsAsFactors = F)
head(d)


#get sf6 dataframe
d <- d%>%
  select("EXPOCODE", "STNNBR", "BTLNBR","SAMPNO", "DATE",  "LATITUDE", "LONGITUDE",
         "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"SF6","SF6_FLAG_W","year")
unique(d$year)

#filter/keep good data usign ocads quality control flags, 2 = good, 6 = median of replicates
d <- d[d$SF6_FLAG_W==2 | d$SF6_FLAG_W==6 | d$SF6_FLAG_W==0,]
d <- d[rowSums(is.na(d)) != ncol(d), ]


#add in 2023 (sf6 not currently in ocads... )
f <- read.csv("E:/A_docs/2023/2023_LabSea/2023_LabSea_allchem_AR7W_sf6.csv", stringsAsFactors = F)
f$SALNTY <- NA
f$SF6_FLAG_W <- NA
f$year <- rep(2023,length(f$X))

f_extracted <- f %>%
  select("cruise_number", "event","trip_number","sample_id", "date",
         "latitude","longitude", "PrDM","T090C", "Sal00", "SALNTY",
         "sf6_fmolperkg","SF6_FLAG_W","year")

colnames(f_extracted) <- c("EXPOCODE", "STNNBR", "BTLNBR","SAMPNO", "DATE",  "LATITUDE", "LONGITUDE",
                           "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"SF6","SF6_FLAG_W","year")

#rbind dataframes
d <- rbind.data.frame(d,f_extracted)

# # lookign at years... 
# d1 <- d[d$year==2018,]
# d2 <- d[d$year==2024,]
# d3 <- d[d$year==2022,]
#  
# plot(d1$SF6,-d1$CTDPRS)
# points(d2$SF6,-d2$CTDPRS, col = "blue")
# points(d3$SF6,-d3$CTDPRS, col = "red")
# 
# deep <- d1[d1$CTDPRS>1900,]

#flags in 2018
#stn. 129 of 2018 are too low
d <- d[d$STNNBR != 129, ]
#stn. 133 of 2018 are too high
d <- d[d$STNNBR != 133, ]


#remove sf6 = 0
d <- d[d$SF6 != 0, ]

unique(d$year)

#find NEADW
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.04),]

neadw_avesf6_ocads <- mean(neadw$SF6)
neadw_sdsf6_ocads <- sd(neadw$SF6)

uyear <- unique(d$year)
s_ne = NULL

for (yr in uyear){
  
  e <- neadw[neadw$year == yr, ]
  
  sf6ave <- mean(e$SF6, na.rm = TRUE)
  sf6sd  <- sd(e$SF6, na.rm = TRUE)
  sf6an  <- sf6ave - neadw_avesf6_ocads
  n <- length(e$EXPOCODE)
  e_df <- data.frame(year = yr,
                     mean = sf6ave,
                     sd = sf6sd,
                     anomaly = sf6an,
                     n = n)
  
  s_ne <- rbind(s_ne, e_df)
}
s_ne <- s_ne[order(s_ne$year), ]

# half_sd <- s_ne$sd/2
# ulim <- s_ne$mean+half_sd
# llim <- s_ne$mean-half_sd
# 
# plot(s_ne$year,s_ne$mean, type = "both", pch = 19, ylim = c(0.5,1.8), xlim = c(2010, 2026), 
#      main = "NEADW timeseries", ylab = "[sf6] (fmol kg-1)", xlab = "year")
# arrows(s_ne$year,llim, s_ne$year,ulim, angle = 90, code = 3, length = 0.05)
# points(s_ne$year,s_ne$anomaly, pch = 19, col = "blue") 

#find NvLSW

nvlsw <- d[d$LATITUDE > 56 & d$LATITUDE <59.1,]
nvlsw <- nvlsw[nvlsw$CTDPRS > 150 & nvlsw$CTDPRS < 500,]

nvlsw_avesf6_ocads <- mean(nvlsw$SF6)
nvlsw_sdsf6_ocads <- sd(nvlsw$SF6)

uyear <- unique(d$year)
s_nv = NULL

for (yr in uyear){
  
  e <- nvlsw[nvlsw$year == yr, ]
  
  sf6ave <- mean(e$SF6, na.rm = TRUE)
  sf6sd  <- sd(e$SF6, na.rm = TRUE)
  sf6an  <- sf6ave - nvlsw_avesf6_ocads
  n <- length(e$EXPOCODE)
  e_df <- data.frame(year = yr,
                     mean = sf6ave,
                     sd = sf6sd,
                     anomaly = sf6an,
                     n = n)
  
  s_nv <- rbind(s_nv, e_df)
}
s_nv <- s_nv[order(s_nv$year), ]


half_sd <- s_nv$sd/2
ulim <- s_nv$mean+half_sd
llim <- s_nv$mean-half_sd

plot(s_nv$year,s_nv$mean, type = "both", pch = 19, ylim = c(0.5,5), xlim = c(2010, 2026), 
     main = "SF6 OCADS Timeseries", ylab = "[sf6] (fmol kg-1)", xlab = "year")
arrows(s_nv$year,llim, s_nv$year,ulim, angle = 90, code = 3, length = 0.05)
lines(c(2012, 2025), c(nvlsw_avesf6_ocads, nvlsw_avesf6_ocads), col = "red", lwd = 1,lty = 2)


points(s_ne$year,s_ne$mean, type = "both", pch = 19,col = "blue")
half_sd <- s_ne$sd/2
ulim <- s_ne$mean+half_sd
llim <- s_ne$mean-half_sd
arrows(s_ne$year,llim, s_ne$year,ulim, angle = 90, code = 3, length = 0.05, col = "blue")
lines(c(2012, 2025), c(neadw_avesf6_ocads, neadw_avesf6_ocads), col = "red", lwd = 1,lty = 2)

legend("topright",
       legend = c("NvLSW - 150<depth<500m, 56<Lat<59.1", 
                  "NEADW - 36.965< σ2<37.04, 56<Lat<60", 
                  "Climatology - mean 2012:2025"),
       pch = c(19),
       col = c("black", "blue", "red"),
       bty = "n")




