rm(list = ls())
library(oce)
library(dplyr)
library(akima)
library(fields)
library(seacarb)

# Load data
# d <- read.csv("E:/A_docs/2025/2025_labSea/LAT2025146_allchem_final.csv")
btm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/btm.csv")  
# d <- d[!is.na(d$TIC..umol.kg.1.) & !is.na(d$TA..umol.kg.1.), ]
# d <- d[d$TIC.Flag == "" & d$TA.Flag == "", ]
# 
# d <- d %>%
#   select("cruise_number", "event",  "station", "date","sample_id",
#          "eventLatitude","eventLongitude", "PrDM","T090C", "Sal00", 
#          "TIC..umol.kg.1.", "TIC.Flag", "TA..umol.kg.1.", "TA.Flag")
# 
# write.csv(d,"data/2025_carbonate_data.csv", row.names = F)


d <- read.csv("data/2025_carbonate_data.csv")


plot(d$eventLongitude,d$eventLatitude)

#get lsw values
lsw <- d[d$eventLatitude > 56 & d$eventLatitude <59.1,]
lsw <- lsw[lsw$PrDM > 200 & lsw$PrDM < 2000,]

mean(lsw$pco2_out_uatm)
sd(lsw$pco2_out_uatm)
mean(lsw$pH_out)
sd(lsw$pH_out)



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


ustn <- unique(d$station)
i <- ustn[7]
for(i in ustn){
  e <- d[which(d$station==i),]
  t <- e$T090C
  ta <- e$TA..umol.kg.1.
  tic <- e$TIC..umol.kg.1.
  pH <- e$pH_out
  pco2 <- e$pco2_uatm
  pH <- e$pH
  stn <- unique(e$station)
  p <- e$PrDM
  par(mfrow = c(1,3), mar = c(5, 2, 3, 1 ))
  plot(tic, -p, xlim = c(2070,2315), ylim = c(-3700,0), pch = 16, main = stn)
  plot(ta, -p, xlim = c(2185,2315), ylim = c(-3700,0),pch = 16)
  
  # plot(pH, -p, xlim = c(7.9, 8.1), ylim = c(-3700,0), pch = 16, main = stn)
  # plot(pco2, -p, xlim = c(300,500), ylim = c(-3700,0),pch = 16)
  plot(t, -p, xlim = c(-1.5, 15), ylim = c(-3700,0),pch = 16)
  # plot(s, -p, xlim = c(30, 36), ylim = c(-3700,0),pch = 16)
}
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

#get lsw values
lsw <- d[d$eventLatitude > 56 & d$eventLatitude <59.1,]
lsw <- lsw[lsw$PrDM > 200 & lsw$PrDM < 2000,]

mean(lsw$pco2_out_uatm)
mean(lsw$pH_out)

