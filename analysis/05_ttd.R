# ==========================================================
# Script: 05_ttd.R
#
# Project:  ttd
# Purpose: compute ttds - gammabest
# 
# Notes:  for now using LRs model based on f12 observed saturation, 
#     Improvements/updates include:
#       obtain lsw sigma2 limits for 2017-2026
#       look at sf6 obs. saturation and rerun model 
#       get MLD data, TS data, currently using digitize data from IY2024
# ==========================================================
library(oce)

source("R/ttd.r")
source("R/compute_Cxt.r")
source("R/build_lookup_table.r")
source("R/estimate_gamma.r")
source("R/f12_plot_gamma.r")
source("R/sf6_plot_gamma.r")

# C_star - input to the system  
cO<- read.csv("data/processed/recon_surf_history.csv")
#approx function
cO_fun_xeff_f12 <- approxfun(cO$f12_sat_model.yr, cO$f12_C_star_xeff, rule = 1)  
cO_fun_xeff_sf6 <- approxfun(cO$f12_sat_model.yr, cO$sf6_C_star_xeff, rule = 1)  

# load tracer data
d_f12 <- read.csv("data/processed/ocads_clean_f12.csv")
d_f12$yr <- as.numeric(substr(d_f12$DATE,1,4))
d_sf6 <- read.csv("data/processed/ocads_clean_sf6.csv")
d_sf6$yr <- as.numeric(substr(d_sf6$DATE,1,4))

f12 <- estimate_gamma(d_f12, "CFC12")
sf6 <- estimate_gamma(d_sf6, "SF6")

f12_plot_gamma(f12)
sf6_plot_gamma(sf6)

write.csv(f12, "data/processed/f12_gamma.csv", row.names = F)
write.csv(sf6, "data/processed/sf6_gamma.csv", row.names = F)


