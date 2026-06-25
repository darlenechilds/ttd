# Compute %Sat for 2016 - 2026
rm(list = ls())
library(oce)
sf6_solubility <- function(t, s) {
  T_k <- t + 273.15 # Convert temp to Kelvin
  sf6_Ln_F <- -82.1639+120.152*(100/T_k)+30.6372*log(T_k/100)+
    s*(0.0293201+-0.0351974*(T_k/100)+0.00740056*(T_k/100)^2)
  sf6_F <- exp(sf6_Ln_F)   # fmol per kg
  return(sf6_F)
}

f12_solubility <- function(t, s) {
  t_k <- t + 273.15 
  f12_ln_F <- -220.2120 + 301.8695 * (100 / t_k) + 114.8533 * log(t_k / 100) + -1.39165 * (t_k / 100)^2 +
    s * (-0.147718 + 0.093175 * (t_k / 100) + -0.0157340 * (t_k / 100)^2)
  f12_F <- exp(f12_ln_F)  
  return(f12_F)
}

lsw_sigma <- data.frame(
  year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
           2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
           2011, 2012, 2013, 2014, 2015, 2016),
  sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
             36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
             36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
             36.879, 36.896))



#load data
# d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv", stringsAsFactors = F)
# d <- d[!is.na(d$SF6) & d$SF6 > 0, ]

# #unsubmitted data
d <- read.csv("data/processed/UNSUB_tracers_o2.csv")

# lookup table
years <- c(
  "HUD2015006" = 2015,
  " HUD2016006" = 2016,
  "HUD2018008" = 2018,
  " AMU2019001" = 2019,
  "AMU2020001" = 2020,
  "AT4805"     = 2022,
  "CAR2023573" = 2023,
  "CAR2024924" = 2024,
  "LAT2025146" = 2025
)

# add year column
d$year <- years[d$EXPOCODE]
d$SA <- gsw_SA_from_SP(d$CTDSAL, d$CTDPRS, d$LONGITUDE, d$LATITUDE)
d$CT <- gsw_CT_from_t(d$SA, d$CTDTMP, d$CTDPRS)
d$sigma2 <- gsw_sigma2(d$SA,d$CT)


mld <- read.csv("data/raw/mld_Fig4_Yashayaev2024.csv")
mld$year <- floor(mld$x)

sf6_ppt <- read.csv("data/processed/sf6_atm.csv")
sf6_ppt$yr <- floor(sf6_ppt$YEAR)
f12_ppt <- read.csv("data/processed/f12_atm.csv")
f12_ppt$yr <- floor(f12_ppt$YEAR)



uyear <- unique(d$year)
s <- NULL
i <- 2015
for (i in 2012:2025){
  mld_cut <- mld$y[mld$year == i]
  # sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == i]
  
  lsw <- d[d$year == i &
             d$LATITUDE > 56 & d$LATITUDE < 59.1 &
             d$CTDPRS > 200 &
             d$CTDPRS < mld_cut, ]
    
  sf6_air <- sf6_ppt[sf6_ppt$yr==i,]
    sf6_air_ppt <- sf6_air$SF6[3]  #ppt
    lsw$sf6_air <- rep(sf6_air_ppt,length(lsw$EXPOCODE))
    f12_air <- f12_ppt[f12_ppt$yr==i,]
    f12_air_ppt <- mean(f12_air$CFC12) #ppt
    lsw$f12_air <- rep(f12_air_ppt,length(lsw$EXPOCODE))
    s <- rbind.data.frame(s,lsw)
    

    }

unique(s$year)

CT_means <- aggregate(CT ~ year, data = s, mean, na.rm = TRUE)
SA_means <- aggregate(SA  ~ year, data = s, mean, na.rm = TRUE)
sigma2_means <- aggregate(sigma2  ~ year, data = s, mean, na.rm = TRUE)
sigma2_max <- aggregate(sigma2  ~ year, data = s, max, na.rm = TRUE)



#sf6 saturation
sf6_F <- sf6_solubility(s$CT,s$SA)   #solubility
s$sf6_C_star <- sf6_F*(s$sf6_air/ 1e12)*1e15  # theoretical [] (fmol per kg() 
s$sat_sf6 <- s$SF6/ s$sf6_C_star *100

obs_sat <- aggregate(sat_sf6 ~ year, data = s, mean, na.rm = TRUE)

u <- s[s$year==2023,]
v <- s[s$year==2024,]
w <- s[s$year==2025,]

plot(u$sat_sf6,-u$CTDPRS, col = "blue", pch = 19, xlim = c(65,100),ylim = c(-800,0))
points(v$sat_sf6,-v$CTDPRS, col = "green", pch = 19)
points(w$sat_sf6,-w$CTDPRS, col = "red", pch = 19)
range(w$sat_sf6)

write.csv(obs_sat, "data/processed/new/obs_sat_new.csv", row.names = F)
