rm(list = ls())
# Compute [CO2] input from pco2 atm, T and S using Weiss 1974 solubility equation

# load pco2 atm from ICE station in Iceland (digitized from Raimondi Fig5)
# d <- read.csv("data/raw/Raimondi_f5_pco2atm.csv", stringsAsFactors = F)  # for purposes of using Raimondi's methods

d <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.csv", stringsAsFactors = F, skip = 37)
d$yr <- floor(d$year)


head(d)
# t and s from 2020 IY et al., csas report
es <- read.csv("data/raw/sal_Fig13_Yashayaev2020.csv",stringsAsFactors = F)
et <- read.csv("data/raw/theta_Fig13_Yashayaev2020.csv",stringsAsFactors = F)
es$yr <- floor(es$x)
et$yr <- floor(et$x)
e <- merge(et,es,by = "yr")  # dropping some data... may have to revisit
e <- cbind.data.frame(e$yr,e$y.x,e$y.y)
colnames(e) <- c("yr","t","s")
head(d)

d <- merge(d, e, by = "yr", all.x = TRUE)
# ## solubility function:  Weiss, 1974, gravimetric - mol per kg per atm
# pco2_solubility <- function(t, s) {
#   t_k <- t + 273.15 
#   pco2_ln_Ko <- -60.2409 + 93.4517 * (100 / t_k) + 23.3585 * log(t_k / 100) +
#     s * (0.023517 - 0.023656 * (t_k / 100) + 0.0047036 * (t_k / 100)^2)
#   pco2_Ko <- exp(pco2_ln_Ko)  
#   return(pco2_Ko)
# }
# d$pco2_f <- pco2_solubility(d$t,d$s)  #  mol per kg per atm
# pco2_p_atm <- d$X / 1e6  #uatm to atm
# d$pco2_c_star <- d$pco2_f*pco2_p_atm * 1e6  #umol per kg  -- Input into teh system


#


write.csv(d,"data/processed/surf_pco2_history.csv", row.names = F)



#single
t <- 0.1
s <- 34.8

pco2_f <- pco2_solubility(t,s)  #  mol per kg per atm

pco2_p_atm <- 400 / 1e6  #uatm to atm
pco2_c_star <- pco2_f*pco2_p_atm * 1e6  #umol per kg  -- Input into teh system
pco2_c_star
