# Compute AOU, AOUR, OCRR, Activation Energy
rm(list = ls())


# input found gamma
d <- read.csv("data/processed/new/gamma_best.csv", stringsAsFactors = F)
head(d)
ind <- which(is.na(d$OXYGEN_mlperl))
d <- d[-ind,]
#get centre lsw
d <- d[d$LATITUDE > 56 & d$LATITUDE < 59.1,]
#get 2025
d <- d[d$year==2025,]
#new thing; gsw can convert stuff!  ie. potential temp, absolute salinity and o2 solubility
p_sea <- d$CTDPRS-10.1325
SA <- gsw_SA_from_SP(d$CTDSAL, p_sea, d$LONGITUDE, d$LATITUDE)
CT <- gsw_CT_from_t(SA, d$CTDTMP, p_sea)
O2sat <- gsw_O2sol(SA, CT, p_sea, d$LONGITUDE, d$LATITUDE)
aou <- O2sat-d$OXYGEN_umolperkg

plot(aou,-p_sea, xlim = c(-50,60))
unique(d$year)
