rm(list = ls())
#compute pmolperkg from Xeff, surface TS, solubility constants

#load f12 modeled saturation 
d <- read.csv("data/processed/f12_sat_modeled.csv")
# e <- read.csv("data/EN4/EN4_TS_surface_labsea.csv")
# e$yr <- floor(e$Year)
# e <- aggregate(cbind(MeanTemp, MeanSal) ~ yr, data = e, FUN = mean, na.rm = TRUE)


# from 2020 IY et al., csas report
es <- read.csv("data/raw/sal_Fig13_Yashayaev2020.csv",stringsAsFactors = F)
et <- read.csv("data/raw/theta_Fig13_Yashayaev2020.csv",stringsAsFactors = F)

es$yr <- floor(es$x)
et$yr <- floor(et$x)
e <- merge(et,es,by = "yr")  # dropping some data... may have to revisit
head(e)
e <- cbind.data.frame(e$yr,e$y.x,e$y.y)
colnames(e) <- c("year","t","s")
d <- merge(d,e,by = "year")  # dropping some data... may have to revisit


#calculate Xeff from modeled f12 sat
d$xeff <- (d$CFC12*d$f12_sat_mod)/100

# # Solubiity function; Bulister et al., 2002, gravimetric - mol per kg per atm
# sf6_solubility <- function(temp, sal) {
#   T_k <- temp + 273.15 # Convert temp to Kelvin
#   sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+
#     sal*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)  
#   sf6_F <- exp(sf6_Ln_F)   # fmol per kg
#   return(sf6_F)
# }

## solubility function:  Warner and Weiss, 1985, gravimetric - mol per kg per atm
f12_solubility <- function(t, s) {
  t_k <- t + 273.15 
  f12_ln_F <- -220.2120 + 301.8695 * (100 / t_k) + 114.8533 * log(t_k / 100) + -1.39165 * (t_k / 100)^2 +
  s * (-0.147718 + 0.093175 * (t_k / 100) + -0.0157340 * (t_k / 100)^2)
  f12_F <- exp(f12_ln_F)  
  return(f12_F)
}

d$f12_F <- f12_solubility(d$t,d$s)

f12_p_atm_xeff <- d$xeff / 1e12
d$f12_C_star_xeff <- d$f12_F*f12_p_atm_xeff*1000000000000  #pmol per kg

f12_p_atm <- d$CFC12 / 1e12
d$f12_C_star <- d$f12_F*f12_p_atm*1000000000000  #pmol per kg


# write.csv(d,"data/processed/recon_surf_f12_history.csv", row.names = F)


