# Compute %Sat for 2016 - 2026
rm(list = ls())
library(oce)
library(akima)

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
  return(d)}

#sigma2 from Raimondi et al., 2023
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
# d_ocads <- d[!is.na(d$SF6) & d$SF6 > 0, ]


# #unsubmitted data
d <- read.csv("data/processed/UNSUB_tracers_o2.csv")
d <- d[order(d$SAMPNO),]
d <- d[-which(is.na(d$SF6)),]


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

#merge 2012 data
# d_ocads <- d_ocads[,-1]
# #
# d1 <- rbind.data.frame(d_ocads,d)

d$SA <- gsw_SA_from_SP(d$CTDSAL, d$CTDPRS, d$LONGITUDE, d$LATITUDE)
d$CT <- gsw_CT_from_t(d$SA, d$CTDTMP, d$CTDPRS)
d$sigma2 <- gsw_sigma2(d$SA,d$CT)

long1 <- -55.5448
lat1 <- 53.6805
pal <- colorRampPalette(c("blue", "lightgrey","red"))


uyear <- unique(d$year)

i <- 2024
#pts for TS plot

for (i in uyear){
  e <- d[d$year==i,]
  e$dist <- earthDist(long1,lat1,e$LONGITUDE,e$LATITUDE)
  
  # file_path <- paste0("output/",i,"_sf6_ar7w_ts.png")
  # png(file_path, width = 800, height = 600, res = 100)
  # 
  # interp_sf6 <- with(e,interp(x = dist,y = CTDPRS,z = SF6, duplicate = "mean"))
  # levels = seq(0, 5, by = 0.1)
  # filled.contour(interp_sf6$x,  interp_sf6$y,  interp_sf6$z,
  #                ylim = rev(range(interp_sf6$y)),
  #                xlim = c(0,950),
  #                zlim = c(0,5),
  #                color.palette = colorRampPalette(
  #                  c("blue", "white", "red")),
  #                xlab = "Distance (km)",
  #                ylab = "Pressure (dbar)")
  # text(100,3500,paste(i,"sf6 (fmol kg-1)"))

  centre <- e[e$LATITUDE > 56 & e$LATITUDE <59.1,]
  # # plot(centre$SF6,centre$sigma2, main = i,
  # #      ylim = rev(c(36.6,37.2)),
  # #      xlim = c(1,5))
  #
  ncol <- 100
  sf6.col <- pal(ncol)
  col.ind <- cut(centre$SF6, breaks = ncol, labels = FALSE)
  plot(centre$SA,centre$CT,
       ylim = c(1,6),
       xlim = c(34,35.1),
       pch = 16, main = i,
       col = sf6.col[col.ind],
       xlab = "Salinity",
       ylab = "Temperature (°C)")
  # dev.off()
  
  }

i <- 2018
e <- d[d$year==i,]
e$dist <- earthDist(long1,lat1,e$LONGITUDE,e$LATITUDE)

centre <- e[e$LATITUDE > 56 & e$LATITUDE <59.1,]
points(centre$CTDSAL,centre$CTDTMP, pch = 16, col = sf6.col[col.ind])


plot(d$CTDSAL,d$CTDTMP, pch = 16)

points(d$CTDSAL,d$CTDTMP, pch = 16, col = d$year)




library(viridis)

yrs <- sort(unique(d$year))

cols <- viridis(length(yrs))

plot(d$CTDSAL, d$CTDTMP,
     pch = 16,
     col = cols[match(d$year, yrs)],
     xlab = "Salinity",
     ylab = "Temperature")

legend("topleft",
       legend = yrs,
       col = cols,
       pch = 16,
       cex = 0.8)

