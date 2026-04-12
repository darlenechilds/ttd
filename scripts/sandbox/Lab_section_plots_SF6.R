rm(list = ls())
library(oce)
library(akima)
library(fields)
library(cmocean)

# Load data
d_all <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/data_raw_shortcuts/2015_2025_lab_tracers.csv")  
btm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/btm.csv")  

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

# Select up to 9 years (or fewer) for the 3x3 plot
years <- sort(unique(d_all$year))
years_to_plot <- years[1:min(9, length(years))]  

# Save to a single PNG 
# png("output/sf6_lab_meas.png", width = 1800, height = 1800, res = 200)
png("output/sf6_lab_meas2018.png", width = 1800, height = 1800, res = 200)

# Set layout and margins
par(mfrow = c(3, 3))
par(cex = 1.2, mar = c(2, 2, 2, 1), oma = c(4, 4, 4, 2), tcl = -0.25, mgp = c(2, 0.6, 0))

year <- years_to_plot[3]

# Loop through each year and add to the panel
for (year in years_to_plot) {
  d <- d_all[d_all$year == year, ]
  if (nrow(d) == 0) next
  
  long1 <- -55.5448
  lat1 <- 53.6805
  d$dist <- earthDist(long1, lat1, d$longitude, d$latitude)
  
  dist <- d$dist
  pres <- -d$PrDM
  z <- as.numeric(d$sf6_fmolperkg)
  
  fld <- with(d, interp(x = dist, y = pres, z = z, duplicate = "strip"))
  
  image.plot(
    x = fld$x, y = fld$y, z = fld$z,
    xlim = c(0, 900), ylim = c(-3700, -10), zlim = c(0, 4),
    xlab = "Distance (km)", ylab = "Pressure (dbar)",  main = paste(as.character(year),"SF6 (fmol kg-1)"),
    cex.main = 2.5, cex.lab = 2, cex.axis = 2,
    legend.mar = 3, legend.width = 1.2,
    col = viridis::viridis(100, option = "D"))
  
  # viridis::viridis(100, option = "D")
  # viridis::magma(100)
  # viridis::inferno(100)
  # viridis::plasma(100)
  # viridis::cividis(100)
  # 
   
  
  
  contour(x = fld$x, y = fld$y, z = fld$z,
          xlim = c(0, 900), ylim = c(-3700, 0),
          nlevels = 15, add = TRUE, drawlabels = FALSE)
  
  points(d$dist, -d$PrDM, pch = 20, cex = 0.5, col = "black")
  polygon(btm$dist, -btm$btm_S, col = "grey")
}

# Global labels
mtext("Distance (km)", side = 1, outer = TRUE, line = 2.5)
mtext("Pressure (dbar)", side = 2, outer = TRUE, line = 2.5)
mtext("SF6 Measurements (fmol kg-1)", side = 3, outer = TRUE, line = 1.5, cex = 1.2)

# Finish PNG
dev.off()
