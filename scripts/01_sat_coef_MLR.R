rm(list = ls())
# Update Xeff usign equation 4 from Raimondi et al., for years 2016-2026 and add to the history from Raimondi et al., 

# calculate/test equation 3 to ensure using right variables
# input = tracers atmospheric mixing ration, MLD 
# CFC = 1986-2016
# SF6 = 2012-2016
# compare results with paper

#input
mld_model <- read.csv("data/raw/Raimondi_MLD_S2.csv", header = T)
colnames(mld_model) <- c("year","mld")
mld_iy <- read.csv("data/raw/mld_Fig4_Yashayaev2024.csv", header = T)

plot(mld_iy$x,mld_iy$y, type = "b", xlim = c(1940,2026))
points(mld_model$year,mld_model$mld, col = "blue", type = "b")

atm_sf6 <- read.csv("data/processed/sf6_atm.csv",header = T)
atm_f12 <- read.csv("data/processed/f12_atm.csv",header = T)

# first derative of the atmospheric input function 
atm_f12$year <- floor(atm_f12$YEAR)
annual <- aggregate(CFC12 ~ year, data = atm_f12, FUN = mean)
annual$dc_dt <- c(NA,diff(annual$CFC12)/diff(annual$year))

atm_sf6$year <- floor(atm_sf6$YEAR)
annual_sf6 <- aggregate(SF6 ~ year, data = atm_sf6, FUN = mean)
annual_sf6$dc_dt <- c(NA,diff(annual_sf6$SF6)/diff(annual_sf6$year))

#dc_dt were added manually to; 
obs_sat <- read.csv("data/raw/Raimondi_obsf12sat_Fig2.csv")

# MLR of observed saturation to est. sat years before f12 and sf6 were measured.  
modeled_sat <- lm(obs_f12_sat ~ dc.dt  + mld_iy, data = obs_sat)
summary(modeled_sat)

# using modeled_sat coefficients, compute %sat for cfc12 and compare with Lorenza's
a <- modeled_sat$coefficients[1]
b <- modeled_sat$coefficients[2]
c <- modeled_sat$coefficients[3]

f <- merge(x = annual, y = mld_model, by = "year")
f$f12_sat_mod <- a + b * f$dc_dt + c * f$mld

plot(f$year,f$f12_sat_mod)
points(obs_sat$yr,obs_sat$obs_f12_sat,col = "red")


write.csv(f,"data/processed/f12_sat_modeled.csv", row.names = F)
