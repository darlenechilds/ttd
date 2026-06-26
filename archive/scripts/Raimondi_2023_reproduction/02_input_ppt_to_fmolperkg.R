rm(list = ls())
#compute fmolperkg from Xeff, surface TS, solubility constants

#load sf6 modeled saturation 
d <- read.csv("data/processed/sf6_sat_modeled.csv")

# from 2020 IY et al., csas report
es <- read.csv("data/raw/sal_Fig13_Yashayaev2020.csv",stringsAsFactors = F)
et <- read.csv("data/raw/theta_Fig13_Yashayaev2020.csv",stringsAsFactors = F)

es$yr <- floor(es$x)
et$yr <- floor(et$x)
e <- merge(et,es,by = "yr")  # dropping some data... may have to revisit
e <- cbind.data.frame(e$yr,e$y.x,e$y.y)
colnames(e) <- c("year","t","s")
d <- merge(d,e,by = "year")  # dropping some data... may have to revisit
tail(d)


#calculate Xeff from modeled f12 sat
d$xeff <- (d$SF6*d$sf6_sat_mod)/100

# Solubiity function; Bulister et al., 2002, gravimetric - mol per kg per atm
sf6_solubility <- function(t, s) {
  T_k <- t + 273.15 # Convert temp to Kelvin
  sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+
    s*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)
  sf6_F <- exp(sf6_Ln_F)   # fmol per kg
  return(sf6_F)
}


d$sf6_F <- sf6_solubility(d$t,d$s)

sf6_p_atm_xeff <- d$xeff / 1e12
d$sf6_C_star_xeff <- d$sf6_F*sf6_p_atm_xeff*1e15  #fmol per kg

sf6_p_atm <- d$SF6 / 1e12
d$sf6_C_star <- d$sf6_F*sf6_p_atm*1e15  #fmol per kg


write.csv(d,"data/processed/recon_surf_sf6_history.csv", row.names = F)


