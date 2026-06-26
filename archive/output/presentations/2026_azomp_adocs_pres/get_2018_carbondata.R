rm(list = ls())
library(oce)
library(dplyr)
library(akima)
library(fields)
library(seacarb)

# get data file from allchem 
# Load data
# d <- read.csv("E:/A_docs/2010s/2018/2018_HUD2018008/HUD2018-008 all chem data flagged 1st Nov 2018.csv")
btm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/btm.csv")  
# get ar7w line
# d$st <- substr(d$Station,1,2)
# unique(d$st)
# d <- d[d$st=="L3",]
# plot(d$longitude,d$latitude)
#remove blank spaces and flagged data
# d <- d[!is.na(d$DIC.FLAG) & !is.na(d$TA_umol.kg), ]
# d <- d[d$DIC.FLAG == "" & d$TA.Flag == "", ]
# d <- d[d$TA.FLAG == "", ]
# d <- d %>%
#   select("cruise", "Event",  "Station", "date","sample_id",
#          "latitude","longitude", "PrDM","T068C", "Sal00", 
#          "DIC_umol.kg", "DIC.FLAG", "TA_umol.kg", "TA.FLAG")
# 
# write.csv(d,"data/2018_carbonate_data.csv", row.names = F)


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


# res <- carb(
#   flag = 15,            # TA & DIC are the input pair
#   var1 = d$TA..umol.kg.1. ,          # TA in µmol/kg
#   var2 = d$TIC..umol.kg.1.,          # DIC in µmol/kg
#   S = d$Sal00,               # salinity
#   T = d$T090C,               # temperature in °C
#   P = d$PrDM                 # pressure in dbar
# )
# d <- cbind(d, res)

#im not trusting seacarb yet... had to do the spred sheet thing
d <- read.csv("data/2018_carbonate_data.csv", header = T)

flags <- c(438045)   
ind <- which(d$sample_id %in% flags)  
d <- d[-ind,] 


#get lsw values
# lsw <- d[d$latitude > 56 & d$latitude <59.1,]
# lsw <- lsw[lsw$PrDM > 200 & lsw$PrDM < 2000,]
# mean(lsw$ph_out)
# sd(lsw$ph_out)
# mean(lsw$pco2_out_uatm)
# sd(lsw$pco2_out_uatm)


# ustn <- unique(d$Station)
# i <- ustn[7]
# for(i in ustn){
#   e <- d[which(d$Station==i),]
#   ta <- e$TA_umol.kg
#   tic <- e$DIC_umol.kg
#   pH <- e$ph_out
#   stn <- unique(e$Station)
#   p <- e$PrDM
#   par(mfrow = c(1,3), mar = c(5, 2, 3, 1 ))
#   plot(ta, -p, xlim = c(2090,2320), ylim = c(-3700,0), pch = 16, main = stn)
#   plot(tic, -p, xlim = c(2010, 2225), ylim = c(-3700,0),pch = 16)
#   plot(pH, -p, xlim = c(7,9), ylim = c(-3700,0),pch = 16)
#   # plot(t, -p, xlim = c(-1.5, 15), ylim = c(-3700,0),pch = 16)
#   # plot(s, -p, xlim = c(30, 36), ylim = c(-3700,0),pch = 16)
# }
# # 
# # 
# # f <- d[d$event==25, ]

year <- "2018"

long1 <- -55.5448
lat1 <- 53.6805
d$dist <- earthDist(long1, lat1, d$longitude, d$latitude)


#pH
dist <- d$dist
pres <- -d$PrDM
z <- as.numeric(d$ph_out)

fld <- with(d, interp(x = dist, y = pres, z = z, duplicate = "strip"))
png("output/pH_2018.png", width = 1800, height = 1800, res = 200)

image.plot(x = fld$x, y = fld$y, z = fld$z,
           xlim = c(0, 900), ylim = c(-3700, -10), zlim = c(7.9,8.17),
           xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year),"pH_total"),
           legend.mar = 3, legend.width = 1.2,
           cex.main = 2.5, cex.lab = 1.5, cex.axis = 1,
          legend.only = FALSE,col = viridis::viridis(100, option = "D"))

contour(x = fld$x, y = fld$y, z = fld$z,
        xlim = c(0, 900), ylim = c(-3700, 0),
        nlevels = 15, add = TRUE, drawlabels = FALSE)

points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
polygon(btm$dist, -btm$btm_S, col = "grey")
dev.off()

#pco2
dist <- d$dist
pres <- -d$PrDM
z <- as.numeric(d$pco2_out_uatm)

fld <- with(d, interp(x = dist, y = pres, z = z, duplicate = "strip"))
png("output/pco2_2018.png", width = 1800, height = 1800, res = 200)

image.plot(x = fld$x, y = fld$y, z = fld$z,
           xlim = c(0, 900), ylim = c(-3700, -10), zlim = c(325,500),
           xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year),"pCO2 (uatm)"),
           legend.mar = 3, legend.width = 1.2, 
           cex.main = 2.5, cex.lab = 2, cex.axis = 2,
           legend.only = FALSE,col = viridis::viridis(100, option = "D"))

contour(x = fld$x, y = fld$y, z = fld$z,
        xlim = c(0, 900), ylim = c(-3700, 0),
        nlevels = 15, add = TRUE, drawlabels = FALSE)

points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
polygon(btm$dist, -btm$btm_S, col = "grey")
dev.off()

#get lsw values
lsw <- d[d$latitude > 56 & d$latitude <59.1,]
lsw <- lsw[lsw$PrDM > 200 & lsw$PrDM < 2000,]

mean(lsw$ph_out)
mean(lsw$pco2_out_uatm)
