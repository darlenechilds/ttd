#  Inspired by Zeliang Wang; Can oxygen measurements be used to calculated the MLD?

rm(list = ls())
library(dplyr)
library(oce)

#load data
d <- read.csv("data/UNSUB_tracers_o2.csv")   #read in tracer data
o2_flag <- which(d$OXYGEN_umolperkg<220)
d <- d[-o2_flag,]
o2 <- d[!is.na(d$OXYGEN_umolperkg), ]  # remove o2 nans

# calculated theoretical DO using Weiss 1970 solubility constants
T_k <- o2$CTDTMP+273.15
s <- o2$CTDSAL
Ln_F <- -173.4965+249.6339*(100/T_k)+143.3483*log(T_k/100)+-21.8492*(T_k/100)+s*(-0.033096+0.014259*(T_k/100)+-0.0017000*(T_k/100)^2)
o2$eF <- exp(Ln_F)
o2$do_sat <- o2$OXYGEN_mlperl/eF*100
o2$aou <- eF-o2$OXYGEN_mlperl

#define stations 14-19 as in Lazier et al., 2002
lsw_centre <- o2[which(o2$LATITUDE > 56.5 & o2$LATITUDE <59),]  
lsw_centre <- lsw_centre[which(lsw_centre$CTDPRS > 200),]  

#ensure no offline (off ar7w) data
plot(lsw_centre$LONGITUDE,lsw_centre$LATITUDE)


plot(lsw_centre$OXYGEN_mlperl,-lsw_centre$CTDPRS)
points(lsw_centre$eF,-lsw_centre$CTDPRS, col = "blue")

unique(lsw_centre$EXPOCODE)

