# ==========================================================
# Script: 02_qc_data.R
#
# Project:  ttd
# Purpose: double check ocads flags/qc data that has not been 
#   flagged by ocads yet. 
# manual process to id outliers, zeros, etc and flag IDs in
# cleaning scripts 
# ==========================================================
library(oce)
source("R/f12_plot_conc.r")
source("R/sf6_plot_conc.r")

d <- read.csv("data/processed/ocads_clean_f12.csv")
d$yr <- substr(d$DATE,1,4)
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

plot(d$CFC12,-d$CTDPRS)

f12_plot_conc(d)

# look at neadw
neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.015),]
neadw_yr <- neadw[neadw$yr==2025,]
plot(neadw$CFC12,-neadw$CTDPRS)
points(neadw_yr$CFC12,-neadw_yr$CTDPRS, col = "red", pch = 16)
# plot(neadw$LONGITUDE,neadw$LATITUDE)

# look at lsw centre - found while plotting gamma...suspect bad meas. in 2018
d <- d[d$LATITUDE > 56 & d$LATITUDE < 60,]
d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
plot(d$LONGITUDE,d$LATITUDE)
plot(d$CFC12,-d$CTDPRS)
# d <- d[d$CTDPRS > 10 & d$CTDPRS < 200,]  
d$yr <- substr(d$DATE,1,4)
uyear <- as.numeric(unique(d$yr))

i <- uyear[26]
for (i in uyear){
  e <- d[d$yr==i,]
  plot(e$CFC12,-e$CTDPRS, pch = 16, main = i, xlim = c(0,4))
  
}

# 2026
ustn <- unique(e$CASTNO)
j <- ustn[8]
for (j in ustn){
  f <- e[e$CASTNO==j,]
  plot(f$SF6,-f$CTDPRS,pch = 16, main = j, xlim = c(0,4))
}



#  sf6 ###################################################################
d <- read.csv("data/processed/ocads_clean_sf6.csv")
d$yr <- substr(d$DATE,1,4)
uyear <- as.numeric(unique(d$yr))
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)
sf6_plot_conc(d)

unique(d$EXPOCODE)
plot(d$SF6,-d$CTDPRS)

# look at neadw
neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.015),]

plot(neadw$SF6,-neadw$CTDPRS)
plot(neadw$LONGITUDE,neadw$LATITUDE)

i <- uyear[6]
for (i in uyear){
  e <- neadw[neadw$yr==i,]
  plot(e$SF6,-e$CTDPRS, pch = 16, main = i, xlim = c(0,4), ylim = c(-3500,-1800))

}

# look at lsw centre - found while plotting gamma...suspect bad meas. in 2018
d <- d[d$LATITUDE > 56 & d$LATITUDE < 60,]
d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
plot(d$LONGITUDE,d$LATITUDE)
plot(d$SF6,-d$CTDPRS)
# d <- d[d$CTDPRS > 10 & d$CTDPRS < 200,]  

d$yr <- substr(d$DATE,1,4)
uyear <- as.numeric(unique(d$yr))

i <- uyear[6]
for (i in uyear){
  e <- d[d$yr==i,]
  plot(e$SF6,-e$CTDPRS, pch = 16, main = i, xlim = c(0,4))
  
}

# 2018
ustn <- unique(e$STNNBR)
j <- ustn[7]
for (j in ustn){
  f <- e[e$STNNBR==j,]
  plot(f$SF6,-f$CTDPRS,pch = 16, main = j, xlim = c(0,4))
}

#  o2 ###################################################################
d <- read.csv("data/processed/ocads_clean_o2.csv")
d$yr <- substr(d$DATE,1,4)
uyear <- as.numeric(unique(d$yr))
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

unique(d$EXPOCODE)
plot(d$OXYGEN,-d$CTDPRS)

# look at neadw
neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.015),]

plot(neadw$OXYGEN,-neadw$CTDPRS)
plot(neadw$LONGITUDE,neadw$LATITUDE)

# look at lsw centre
d <- d[d$LATITUDE > 56 & d$LATITUDE < 60,]
d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
plot(d$LONGITUDE,d$LATITUDE)
i <- uyear[6]
for (i in uyear){
  e <- d[d$yr==i,]
  plot(e$OXYGEN,-e$CTDPRS, pch = 16, main = i, xlim = c(170,450))
  
}
