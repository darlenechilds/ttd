#  get carbon data for AR7W
rm(list = ls())
library(dplyr)
library(oce)

d <- read.csv("data/processed/OCADS_carbon_ar7w.csv", stringsAsFactors = F)
d <- d[d$ALKALI_FLAG_W==2 ,]  
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)

yr <- 2000
lsw_sigma <- data.frame(
  year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
           2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
           2011, 2012, 2013, 2014, 2015, 2016),
  sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
             36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
             36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
             36.879, 36.896))

sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == yr]

lsw <- d[d$year == yr &
           d$LATITUDE > 56 & d$LATITUDE < 59.1 &
           d$CTDPRS > 200 &
           d$sigma2 < sigma_cut, ]



C_obs_lsw_mean <- mean(lsw$, na.rm = TRUE)
C_obs_lsw_sd <- sd(lsw$cfc12, na.rm = TRUE)
