# ==========================================================
# Script: 02_qc_data.R
#
# Project:  ttd
# Purpose: double check ocads flags/qc data that has not been 
#   flagged by ocads yet. 
# ==========================================================
library(oce)
d <- read.csv("data/processed/ocads_clean_f12.csv")
plot(d$CFC12,-d$CTDPRS)
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

# look at neadw
neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.015),]

plot(neadw$CFC12,-neadw$CTDPRS)
plot(neadw$LONGITUDE,neadw$LATITUDE)

# look at lsw centre - found while plotting gamma...suspect bad meas. in 2018
d <- d[d$LATITUDE > 56 & d$LATITUDE < 60,]
d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
plot(d$LONGITUDE,d$LATITUDE)
plot(d$CFC12,-d$CTDPRS)
d <- d[d$CTDPRS > 10 & d$CTDPRS < 200,]  



d <- read.csv("data/processed/ocads_clean_sf6.csv")
unique(d$EXPOCODE)
plot(d$SF6,-d$CTDPRS)

d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

# look at neadw
neadw <- d[which(d$LATITUDE > 56 & d$LATITUDE <60),]
neadw <- neadw[which(neadw$sigma2 > 36.965 & neadw$sigma2 < 37.015),]

plot(neadw$SF6,-neadw$CTDPRS)
plot(neadw$LONGITUDE,neadw$LATITUDE)

# look at lsw centre - found while plotting gamma...suspect bad meas. in 2018
d <- d[d$LATITUDE > 56 & d$LATITUDE < 60,]
d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
plot(d$LONGITUDE,d$LATITUDE)
plot(d$SF6,-d$CTDPRS)
d <- d[d$CTDPRS > 10 & d$CTDPRS < 200,]  



