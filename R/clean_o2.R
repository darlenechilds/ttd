# ==========================================================
# Function: clean_O2()
# Project: ttd 
# ==========================================================
clean_o2 <- function(d) {
  
  d <- d[c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
      "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY","SALNTY_FLAG_W",
      "CTDOXY","CTDOXY_FLAG_W","OXYGEN","OXYGEN_FLAG_W")]
  numeric_cols <- c("STNNBR","LATITUDE", "LONGITUDE", "CTDPRS", "CTDTMP", "CTDSAL",
    "SALNTY", "CTDOXY", "OXYGEN")
  d[numeric_cols] <- lapply(d[numeric_cols], as.numeric)
  d <- d[d$OXYGEN_FLAG_W==2 | d$OXYGEN_FLAG_W==6, ]

  #remove do = 0, -999
  d$OXYGEN[d$OXYGEN == -999] <- NA
  d <- d[!is.na(d$OXYGEN), ]
  
  # 2022 do data in ml/l
  # get 2022 data
  d1 <- d[d$EXPOCODE=="33AT20220504",]
  #convert DO ml/l to umol/kg (1 kg/m^3 = 0.001 kg/L) 
  theta <- swTheta(d1$CTDSAL, d1$CTDTMP, d1$CTDPRS, referencePressure = 0)
  dens <- (swRho(d1$CTDSAL,theta,d1$CTDPRS))/1000  # kg/L
  conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volume, 1 mole of O2 = 22.4 L, 1/22.3916*10^6)
  d1$OXYGEN <- d1$OXYGEN*conv/dens
  d1$CTDOXY <- d1$CTDOXY*conv/dens
    # remove 2022
  d <- d[!d$EXPOCODE=="33AT20220504",]
  # add 2022 back in
  d <- rbind.data.frame(d,d1)
 
  # ctd salinity missing for 1997
  d2 <- d[d$EXPOCODE=="  18HU19970509",]
  d2$CTDSAL <- d2$SALNTY
  d2$CTDSAL[d2$CTDSAL == -999] <- NA
  d2 <- d2[!is.na(d2$CTDSAL), ]
  d <- d[!d$EXPOCODE=="  18HU19970509",]
  # add 1997 back in
  d <- rbind.data.frame(d,d2)
  
  #outliers found while looking at central Labrador sea
  #2012
  d <- d[!d$SAMPNO %in% c(380640, 380234),]
  
  #2018
  d <- d[!d$SAMPNO %in% c(),]
  
  
  
  return(d)
}
