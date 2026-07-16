# ==========================================================
# Script: 04_compute_input function.R
#
# Project:  ttd
# Purpose: compute input function based on time varying saturation
#   - reconstruct [cfc12], and [sf6] surface history, using time varying saturation, 
#       historical CT and SA from IY2024, solubility constants
# 
# Notes:  for now using LRs model based on f12 observed saturation, 
#     Improvements/updates include:
#       obtain lsw sigma2 limits for 2017-2026
#       look at sf6 obs. saturation and rerun model , for now use % sat from f12 model
#       get MLD data, TS data, currently using digitize data from IY2024, also need recent data (2024-2026)
# ==========================================================

source("R/f12_solubility.r")
source("R/sf6_solubility.r")

f12_sat_model <- read.csv("data/processed/tv_f12_saturation.csv")  #includes f12 atm mixing ratios1

sf6_atm <- read.csv("data/processed/noaa_sf6_atm.csv")
#to match yrs with f12
sf6_pre <- data.frame(YEAR = c(1949, 1950, 1951, 1952),SF6 = c(0, 0, 0, 0))
sf6_atm <- rbind(sf6_pre,sf6_atm)
sf6_atm$yr <- floor(sf6_atm$YEAR)
#ave years
sf6_atm <- aggregate(SF6 ~ yr, data = sf6_atm, FUN = mean) 
#add sf6 mixing ratios to f12_sat 
f12_sat_model <- merge(f12_sat_model,sf6_atm,by = "yr", all.x = TRUE)

# from 2024 IY et al., for 200 - 2000m 
es <- read.csv("archive/data/raw/sal_Fig8_Yashayaev2023.csv",stringsAsFactors = F)
et <- read.csv("archive/data/raw/CT_Fig8_Yashayaev2023.csv",stringsAsFactors = F)

es$yr <- floor(es$x)
et$yr <- floor(et$x)
e <- merge(et,es,by = "yr")  # dropping some data... may have to revisit
e <- e[,c("yr","y.x","y.y")]
names(e) <- c("yr","CT","SA")

#calculate Xeff, effective mole fraction (ppt) of f12 (equ 4 of LR2023)
xeff_f12 <- (f12_sat_model$CFC12*f12_sat_model$f12_sat_mod)/100
xeff_sf6 <- (f12_sat_model$SF6*f12_sat_model$f12_sat_mod)/100
# const. saturation of 80%
# xeff_f12 <- (f12_sat_model$CFC12*f12_sat_model$const_sat)/100
# xeff_sf6 <- (f12_sat_model$SF6*f12_sat_model$const_sat)/100



#calculate solubility
f12_F <- f12_solubility(e$CT,e$SA)
sf6_F <- sf6_solubility(e$CT,e$SA)


#calculate Cstar using the Xeff
f12_p_atm_xeff <- xeff_f12 / 1e12
f12_C_star_xeff <- f12_F*f12_p_atm_xeff*1e12  #pmol per kg

sf6_p_atm_xeff <- xeff_sf6 / 1e12
sf6_C_star_xeff <- sf6_F*sf6_p_atm_xeff*1e15  #fmol per kg

d <- cbind.data.frame(f12_sat_model$yr,f12_C_star_xeff, sf6_C_star_xeff)

write.csv(d,"data/processed/recon_surf_history.csv", row.names = F)
