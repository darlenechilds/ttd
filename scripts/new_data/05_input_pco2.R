# Compute [CO2] input from pco2 atm, T and S using Weiss 1974 solubility equation

rm(list = ls())
library(zoo)
library(seacarb)

# load pco2 atm from ICE station in Iceland (digitized from Raimondi Fig5)
# d <- read.csv("data/raw/Raimondi_f5_pco2atm.csv", stringsAsFactors = F)  # for purposes of using Raimondi's methods

d <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.csv", stringsAsFactors = F, skip = 37)
d$yr <- floor(d$year)


head(d)
# t and s from 2020 IY et al., csas report
es <- read.csv("data/raw/sal_Fig8_Yashayaev2023.csv",stringsAsFactors = F)
et <- read.csv("data/raw/theta_Fig8_Yashayaev2023.csv",stringsAsFactors = F)
es$yr <- floor(es$x)
et$yr <- floor(et$x)
e <- merge(et,es,by = "yr")  # dropping some data... may have to revisit
e <- cbind.data.frame(e$yr,e$y.x,e$y.y)
colnames(e) <- c("yr","t","s")
head(d)

d <- merge(d, e, by = "yr", all.x = TRUE)


#fill in missing temp., sal.
d$t <- na.approx(d$t, x = d$yr, na.rm = FALSE)
d$s <- na.approx(d$s, x = d$yr, na.rm = FALSE)

d$t[1] <- d$t[2]
d$s[1] <- d$s[2]

ta <- (41.25 * d$s + 862.41)/1e6

carb_t <- carb(24, d$mean, ta, d$s,d$t) # cal chem in time
dic_t <- carb_t$DIC
carb_pi <- carb(24, 280, ta, d$s,d$t) #cal chem pre industrial 
dic_pi <- carb_pi$DIC

d$dic_cant <- (dic_t-dic_pi) * 1e6
head(d)
write.csv(d,"data/processed/surf_pco2_history.csv", row.names = F)


