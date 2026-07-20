# ==========================================================
# Script: 05_col_inventory_obs.R
#
# Project:  ttd
# Purpose: compute inventories for observations, sf6, f12, o2 for 
#   the central Labrador sea (>3300m), 
# Method;  obs. are objectively mapped onto a standard grid (spatial res. 5km), 
#   these were averaged horizontally for 5 m intervals,
#   an average profile was then integrated vertically to obtain column inventories
# ==========================================================
library(akima)
library(fields)
library(oce)

source("R/compute_inventory_year.r")

# load tracer data
d_f12 <- read.csv("data/processed/ocads_clean_f12.csv")
d_f12$yr <- as.numeric(substr(d_f12$DATE,1,4))
d_sf6 <- read.csv("data/processed/ocads_clean_sf6.csv")
d_sf6$yr <- as.numeric(substr(d_sf6$DATE,1,4))
d_o2 <- read.csv("data/processed/ocads_clean_o2.csv")
d_o2$yr <- as.numeric(substr(d_o2$DATE,1,4))

s <- d_o2
# as in raimondi get central lab sea, stn with btm depth of >3300
stn_3300 <- s[s$CTDPRS>=3300,]
stn_lswc <- unique(stn_3300$STNNBR)
s <- s[s$STNNBR %in% stn_lswc, ]
s$rho <- swRho(s$CTDSAL,s$CTDTMP,s$CTDPRS)

#remove surf... for o2
s <- s[s$CTDPRS>200,]

uyr <- unique(s$yr)
w <- data.frame()

for (i in uyr) {
  inv <- compute_inventory_year(s,i,tracer = "OXYGEN"  )
  w <- rbind(w, data.frame(year = i,inventory = inv))
}

w <- w[order(w$year),]
plot(w$year,w$inventory,pch = 16, type = "b", main = "O2 Column inventory, 200m-btm, lat_mean_bw: 56-60",
     ylab = "Inv. O2 (umol m-2)", xlab = "Year")


write.csv(w,"data/processed/col_inv_o2.csv")
