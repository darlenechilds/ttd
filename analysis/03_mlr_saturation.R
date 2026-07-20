# ==========================================================
# Script:   03_mlr_saturation.R
#
# Project:  ttd
# Purpose: use MLR model of observed saturation to estimate sat. of f12 and sf6 b/f
#   tracers were measured,  dependent = obs sat, 
#   indep. = 1st derv. (dc/dt) of atm. input & max. MLD 
# Outputs:  data/intermed/
# 
# Notes:  for now using LRs model based on f12 observed saturation, 
#     Improvments/updates include:
#       obtain lsw sigma2 limits for 2017-2026
#       look at sf6 atm input, cal solubilites and rerun model 
#       get MLD data, currently using digitize data from IY2024
 # ==========================================================
library(oce)
source("R/f12_solubility.r")
source("R/get_lsw_f12sat.r")
source("R/get_df12_dt.r")

atm_f12 <- read.csv("data/processed/noaa_f12_atm.csv")
f12 <- read.csv("data/processed/ocads_clean_f12.csv")
# diditized mld4 model from Raimondi et al., 2023, Fig S2
mld4 <- read.csv("archive/data/raw/Raimondi_MLD_S2.csv", header = F)
names(mld4) <- c("yr","mld")

sat_f12_obs <- get_lsw_f12sat(f12)
f12_dt <- get_df12_dt(atm_f12)
mld <- read.csv("archive/data/raw/mld_Fig4_Yashayaev2024.csv")
mld$yr <- floor(mld$x)

d <- merge(sat_f12_obs,f12_dt,by = "yr", all.x = TRUE)
d <- merge(d,mld,by = "yr", all.x = TRUE)
d <- d[c("yr","f12_sat","dc_dt","y")]

modeled_sat <- lm(f12_sat ~ dc_dt  + y, data = d)
summary(modeled_sat)

# using modeled_sat coefficients, compute %sat for cfc12 and compare with Lorenza's
a <- modeled_sat$coefficients[1]
b <- modeled_sat$coefficients[2]
c <- modeled_sat$coefficients[3]

# model Fsat
f <- merge(mld4, f12_dt, by = "yr")
f$f12_sat_mod <- a + b * f$dc_dt + c * f$mld
f$const_sat <- rep(80, length(f$yr))

plot(f$yr,f$f12_sat_mod, ylim = c(30,110))
points(d$yr,d$f12_sat, col = "blue")
points(f$yr,f$const_sat,col = "green")

write.csv(f,"data/processed/tv_f12_saturation.csv")
