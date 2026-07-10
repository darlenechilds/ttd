 # ==========================================================
# Function: sf6_plot_gamma.R
#
# Project:  ttd
# Purpose: plot gamma for the timeseries for each water mass for the LSWcentre
#       surf = 1-200 dbar   
#       lsw_int = 200 - 2000 dbar  - for now later use  LSW = >200m - LSWsigma2
#                                                       $ DIW = LSWsigma2 > sigma2 < 36.96
#       NEADW = 36.96 > sigma2 <  37.1
#       DSOW = bottom 200m
# 
# Notes:  for now using LRs model based on f12 observed saturation, 
#     Improvements/updates include:
#       obtain lsw sigma2 limits for 2017-2026
#       look at sf6 obs. saturation and rerun model 
#       get MLD data, TS data, currently using digitize data from IY2024
# ==========================================================

 sf6_plot_gamma <- function(sf6){
   
     
   # get lsw centre
   d <- sf6[sf6$LATITUDE > 56 & sf6$LATITUDE < 60,]
   d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
   
   # compute sigma2
   d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
   d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)
   
   #get water masses
   surf <- d[d$CTDPRS > 10 & d$CTDPRS < 200,]
   lsw_int <- d[d$CTDPRS > 200 & d$CTDPRS < 2000,]
   neadw <- d[d$sigma2 > 36.965 & d$sigma2 < 37.015,]
   dsow <- d[d$CTDPRS > 3480,]
   
   #means for each water mass for each year
   surf_ave <- aggregate(Gamma ~ yr, data = surf, FUN = mean)
   lsw_int_ave <- aggregate(Gamma ~ yr, data = lsw_int, FUN = mean)
   neadw_ave <- aggregate(Gamma ~ yr, data = neadw, FUN = mean)
   dsow_ave <- aggregate(Gamma ~ yr, data = dsow, FUN = mean)
   
   #plot each water mass
   plot(surf_ave$yr,surf_ave$Gamma, ylim = c(0,160), pch = 16, type = "b", xlab = "Year", ylab = "Gamma (years)")
   points(lsw_int_ave$yr,lsw_int_ave$Gamma, col = "blue", pch = 16, type = "b")
   points(neadw_ave$yr,neadw_ave$Gamma, col = "darkgreen", pch = 16, type = "b")
   points(dsow_ave$yr,dsow_ave$Gamma, col = "darkred", pch = 16, type = "b")
   
   legend("topleft",
          legend = c("Surface", "LSW intermediate", "NEADW", "DSOW"),
          col = c("black", "blue", "darkgreen", "darkred"),
          pch = 16,
          bty = "n")
 }