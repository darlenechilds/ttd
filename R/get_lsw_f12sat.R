# ==========================================================
# Function: get_lsw_f12sat()
# Project: ttd
# Purpose: Calculate % saturation for f12 for the lsw
# input: data/processed/ocads_clean_f12.csv
# ==========================================================

get_lsw_f12sat <- function(f12) {
  
    
  atm_f12$yr <- floor(atm_f12$YEAR)
  
  # from LRs supporting material table S3
  lsw_sigma <- data.frame(
    year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
             2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
             2011, 2012, 2013, 2014, 2015, 2016),
    sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
               36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
               36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
               36.879, 36.896))
  
  
  f12$yr <- substr(f12$DATE,1,4)
  f12$SA <- gsw_SA_from_SP(f12$CTDSAL, f12$CTDPRS, f12$LONGITUDE, f12$LATITUDE)
  f12$CT <- gsw_CT_from_t(f12$SA, f12$CTDTMP, f12$CTDPRS)
  f12$sigma2 <- gsw_sigma2(f12$SA,f12$CT)
  
  uyr <- unique(f12$yr)
  s <- NULL
  
  for (i in uyr){
    sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == i]
    
    lsw <- f12[f12$yr == i &
               f12$LATITUDE > 56 & f12$LATITUDE < 59.1 &
               f12$CTDPRS > 200 &
               f12$sigma2 < sigma_cut, ]
    
    f12_air <- atm_f12[atm_f12$yr==i,]
    f12_air_ppt <- mean(f12_air$CFC12) #ppt
    lsw$f12_air <- rep(f12_air_ppt,length(lsw$EXPOCODE))
    s <- rbind.data.frame(s,lsw)
  }
  
  s$f12_f <-f12_solubility(s$CT,s$SA)
  s$f12_C_star <- s$f12_f*(s$f12_air/ 1e12)*1e12
  s$f12_sat <- s$CFC12/s$f12_C_star*100
  
  
  obs_sat <- aggregate(f12_sat ~ yr, data = s, mean, na.rm = TRUE)
  return(obs_sat)
  
}