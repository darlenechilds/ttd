#  look at how % sat changes using the ocads TS and IY's TS  and compare to LRs obs. f12

library(oce)
sf6_ppt <- read.csv("data/processed/noaa_sf6_atm.csv")
sf6_ppt$yr <- floor(sf6_ppt$YEAR)

d <- read.csv("data/processed/ocads_clean_sf6.csv")
iy_ct <- read.csv("archive/data/raw/CT_Fig8_Yashayaev2023.csv")

lsw_sigma <- data.frame(
  year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
           2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
           2011, 2012, 2013, 2014, 2015, 2016),
  sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
             36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
             36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
             36.879, 36.896))


head(d)
d$year <- substr(d$DATE,1,4)
d$SA <- gsw_SA_from_SP(d$CTDSAL, d$CTDPRS, d$LONGITUDE, d$LATITUDE)
d$CT <- gsw_CT_from_t(d$SA, d$CTDTMP, d$CTDPRS)
d$sigma2 <- gsw_sigma2(d$SA,d$CT)

uyr <- unique(d$year)
s <- NULL
i <- uyr[3]
for (i in uyr){
  sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == i]
  
  lsw <- d[d$year == i &
             d$LATITUDE > 56 & d$LATITUDE < 59.1 &
             d$CTDPRS > 200 &
             d$sigma2 < sigma_cut, ]
  
  sf6_air <- sf6_ppt[sf6_ppt$yr==i,]
  sf6_air_ppt <- sf6_air$SF6[3]  #ppt
  lsw$sf6_air <- rep(sf6_air_ppt,length(lsw$EXPOCODE))
  s <- rbind.data.frame(s,lsw)
}

s$sf6_f <-sf6_solubility(s$CT,s$SA)
s$sf6_C_star <- s$sf6_f*(s$sf6_air/ 1e12)*1e15
s$sf6_sat <- s$SF6/s$sf6_C_star*100


obs_sat <- aggregate(sf6_sat ~ year, data = s, mean, na.rm = TRUE)

#compare with LRs work
lr <- read.csv("archive/data/raw/Raimondi_obs_sf6_sat_Fig3.csv")
plot(lr$yr,lr$obs_sf6_sat,ylim = c(40,100))
points(obs_sat$year,obs_sat$sf6_sat, col = "red")

