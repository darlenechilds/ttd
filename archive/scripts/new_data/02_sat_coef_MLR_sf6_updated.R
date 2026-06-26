rm(list = ls())
# Update Xeff usign equation 4 from Raimondi et al., for years 2016-2026 and add to the history from Raimondi et al., 

#input
mld_model <- read.csv("data/raw/Raimondi_MLD_S2.csv", header = T)
colnames(mld_model) <- c("year","mld")
mld_iy <- read.csv("data/raw/mld_Fig4_Yashayaev2024.csv", header = T)
mld_iy$yr <- floor(mld_iy$x)

# plot(mld_iy$x,mld_iy$y, type = "b", xlim = c(1940,2026))
# points(mld_model$year,mld_model$mld, col = "blue", type = "b")

atm_sf6 <- read.csv("data/processed/sf6_atm.csv",header = T)

# first derivative of the atmospheric input function 
atm_sf6$year <- floor(atm_sf6$YEAR)
annual <- aggregate(SF6 ~ year, data = atm_sf6, FUN = mean)
annual$dc_dt <- c(NA,diff(annual$SF6)/diff(annual$year))

# add dc_dt to obs sat; 
obs_sat_LR <- read.csv("data/raw/Raimondi_obs_sf6_sat_Fig3.csv")
obs_sat_LR$yr <- round(obs_sat_LR$yr)
obs_sat_recent <- read.csv("data/processed/new/obs_sat_new.csv")

obs_sat <- obs_sat_recent[, c("year", "sat_sf6")]
names(obs_sat) <- c("yr", "obs_sf6_sat")
obs_sat <- rbind.data.frame(obs_sat_LR,obs_sat)

e <- annual[annual$year %in% 2012:2025, ]
obs_sat$dc.dt <- e$dc_dt
#merge with mld_IY
i <- mld_iy[mld_iy$yr %in% 2012:2025,]
obs_sat$mld_iy <- i$y

# write.csv(obs_sat,"data/processed/new/sf6_sat_obs_new.csv", row.names = F)

# obs_sat <- obs_sat[-(1:3),]

# MLR of observed saturation to est. sat years before f12 and sf6 were measured.  
modeled_sat <- lm(obs_sf6_sat  ~ dc.dt  + mld_iy, data = obs_sat)
summary(modeled_sat)

# using modeled_sat coefficients, compute %sat for cfc12 and compare with Lorenza's
a <- modeled_sat$coefficients[1]
b <- modeled_sat$coefficients[2]
c <- modeled_sat$coefficients[3]

f <- merge(x = annual, y = mld_model, by = "year")
f$sf6_sat_mod <- a + b * f$dc_dt + c * f$mld

plot(f$year,f$sf6_sat_mod, type = "b", pch = 16,ylim = c(55,100), xlim = c(2000,2025))
points(obs_sat$yr,obs_sat$obs_sf6_sat,col = "red", pch = 16)


write.csv(f,"data/processed/new/sf6_sat_modeled_new.csv", row.names = F)
