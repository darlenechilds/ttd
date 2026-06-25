##  O2 
rm(list = ls())
library(oce)
library(akima)

d <- read.csv("https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0283442/GLODAPv2.2023_Atlantic_Ocean.csv")
d <- subset(d,G2latitude >= 55 &
              G2latitude <= 62 &
              G2longitude >= -54.5 &
              G2longitude <= -48)
plot(d$G2longitude,d$G2latitude)


d <- read.csv("data/processed/OCADS_tracers_o2_ar7w.csv")
d <- d[order(d$SAMPNO),]


d <- d[-which(is.na(d$OXYGEN)),]

for (i in uyear){
  e <- o2[o2$year==i,]
  e$dist <- earthDist(long1,lat1,e$LONGITUDE,e$LATITUDE)
  
  interp_sf6 <- with(e,interp(x = dist,y = CTDPRS,z = OXYGEN_umolperkg , duplicate = "mean"))
  
  # levels = seq(0, 4, by = 0.1)
  
  filled.contour(interp_sf6$x,  interp_sf6$y,  interp_sf6$z,
                 ylim = rev(range(interp_sf6$y)),
                 xlim = c(0,950),
                 zlim = c(100,450),
                 color.palette = colorRampPalette(
                   c("blue", "white", "red")),
                 main = i,
                 xlab = "Distance (km)",
                 ylab = "Pressure (dbar)"
  )
  
}
