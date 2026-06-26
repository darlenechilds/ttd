rm(list = ls())
library(oce)
library(dplyr)
library(akima)
library(fields)
library(seacarb)

# Load data
d <- read.csv("data/2025_carbonate_data_roi.csv")

head(d)
# btm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/btm.csv")  

earthDist <- function (lon1, lat1, lon2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# #get ROIs , NR, north region of interest, SR, south region of interest, CR centre region of intererst
# d$st <- substr(d$station,1,2)
# unique(d$st)
# d1 <- d[d$st=="NR",]
# d2 <- d[d$st=="SR",]
# d3 <- d[d$st=="CR",]
# d <- rbind.data.frame(d1,d2,d3)
# plot(d$eventLongitude,d$eventLatitude)

#convert DO ml/l to umol/kg - (1 kg/m^3 = 0.001 kg/L)
theta <- swTheta(d$Sal00, d$T090C, d$PrDM, referencePressure = 0)
dens <- (swRho(d$Sal00,theta,d$PrDM))/1000  # kg/L
conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volumn, 1 mole of O2 = 22.4 L, 1/22.3916*10^6)
d$do_umolperkg <- d$do_mlperl*conv/dens

#calculate aou
#cal saturations
T_k <- d$T090C+273.15
s <- d$Sal00
d$Ln_F <- -173.4965+249.6339*(100/T_k)+143.3483*log(T_k/100)+-21.8492*(T_k/100)+s*(-0.033096+0.014259*(T_k/100)+-0.0017000*(T_k/100)^2)
d$eF_mlperl <- exp(d$Ln_F)
# do_sat <- d$DO/eF*100
d$aou <- d$eF-d$do_mlperl
#convert from ml per l to umol per kg
dens <- (swRho(d$Sal00,theta,d$PrDM))/1000  # kg/L
conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volumn, 1 mole of O2 = 22.4 L)
d$eF_umolperkg <- d$eF_mlperl*conv/dens


ustn <- unique(d$station)
i <- ustn[7]
for(i in ustn){
  e <- d[which(d$station==i),]
  ta <- e$TA..umol.kg.1.
  tic <- e$TIC..umol.kg.1.
  o2meas <- e$do_umolperkg
  o2sat <- e$eF_umolperkg
  pH <- e$pH
  pco2 <- e$pco2_uatm
  stn <- unique(e$station)
  p <- e$PrDM
  par(mfrow = c(1,3), mar = c(5, 2, 3, 1 ))
  # plot(ta, -p, xlim = c(2090,2320), ylim = c(-3700,0), pch = 16, main = stn)
  # plot(tic, -p, xlim = c(2010, 2225), ylim = c(-3700,0),pch = 16)
  plot(o2meas, -p, xlim = c(240,420), ylim = c(-800,0),pch = 16,main = stn)
  points(o2sat,-p, pch = 16, col = "blue")
  plot(pH, -p, xlim = c(7.8,8.2), ylim = c(-800,0),pch = 16)
  plot(pco2, -p, xlim = c(290,605), ylim = c(-800,0),pch = 16)
  # plot(t, -p, xlim = c(-1.5, 15), ylim = c(-3700,0),pch = 16)
  # plot(s, -p, xlim = c(30, 36), ylim = c(-3700,0),pch = 16)
}
# 
# 
# f <- d[d$event==25, ]

year <- "2025"

long1 <- -55.5448
lat1 <- 53.6805
d$dist <- earthDist(long1, lat1, d$eventLongitude, d$eventLatitude)


#tic 
dist <- d$dist
pres <- -d$PrDM
z <- as.numeric(d$TIC..umol.kg.1.)
  
fld <- with(d, interp(x = dist, y = pres, z = z, duplicate = "strip"))
  
image.plot(x = fld$x, y = fld$y, z = fld$z,
             xlim = c(0, 900), ylim = c(-3700, -10), zlim = c(2100,2181),
             xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year),"TIC (umol kg-1)"),
             legend.mar = 3, legend.width = 1.2, legend.only = FALSE)
  
contour(x = fld$x, y = fld$y, z = fld$z,
          xlim = c(0, 900), ylim = c(-3700, 0),
          nlevels = 15, add = TRUE, drawlabels = FALSE)
  
  points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
  polygon(btm$dist, -btm$btm_S, col = "grey")

#ta
  dist <- d$dist
  pres <- -d$PrDM
  z <- as.numeric(d$TA..umol.kg.1.)
  
  fld <- with(d, interp(x = dist, y = pres, z = z, duplicate = "strip"))
  
  image.plot(x = fld$x, y = fld$y, z = fld$z,
             xlim = c(0, 900), ylim = c(-3700, -10), zlim = c(2200,2325),
             xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year),"TA (umol kg-1)"),
             legend.mar = 3, legend.width = 1.2, legend.only = FALSE)
  
  contour(x = fld$x, y = fld$y, z = fld$z,
          xlim = c(0, 900), ylim = c(-3700, 0),
          nlevels = 15, add = TRUE, drawlabels = FALSE)
  
  points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
  polygon(btm$dist, -btm$btm_S, col = "grey")
  
#pH
s <- read.csv("E:/A_docs/2026/2026_azomp_adocs_pres/2025_carbon.csv")

  dist <- s$dist
  pres <- -s$PrDM
  z <- as.numeric(s$pH.out)
  fld <- with(s, interp(x = dist, y = pres, z = z, duplicate = "strip"))
  
  image.plot(x = fld$x, y = fld$y, z = fld$z,
             xlim = c(0, 900), ylim = c(-3700, -10), 
             xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year),"pH_total Surface"),
             legend.mar = 3, legend.width = 1.2, legend.only = FALSE,
             col = viridis::viridis(100, option = "D"))
  
  contour(x = fld$x, y = fld$y, z = fld$z,
          xlim = c(0, 900), ylim = c(-3700, 0),
          nlevels = 15, add = TRUE, drawlabels = FALSE)
  
  points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
  polygon(btm$dist, -btm$btm_S, col = "grey")

#pCO2
  s <- read.csv("E:/A_docs/2026/2026_azomp_adocs_pres/2025_carbon.csv")
  
  
  dist <- s$dist
  pres <- -s$PrDM
  z <- as.numeric(s$pCO2.out..matm.)
  
  fld <- with(s, interp(x = dist, y = pres, z = z, duplicate = "strip"))
  
  image.plot(x = fld$x, y = fld$y, z = fld$z,
             xlim = c(0, 900), ylim = c(-500, -10), zlim = c(325,525),
             xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year),"pCO2 (uatm)"),
             legend.mar = 3, legend.width = 1.2, legend.only = FALSE)
  
  contour(x = fld$x, y = fld$y, z = fld$z,
          xlim = c(0, 900), ylim = c(-3700, 0),
          nlevels = 15, add = TRUE, drawlabels = FALSE)
  
  points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
  polygon(btm$dist, -btm$btm_S, col = "grey")
  
  