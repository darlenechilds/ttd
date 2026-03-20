rm(list = ls())
library(oce)
library(akima)
library(fields)
library(cmocean)

# Load data
d <- read.csv("data/UNSUB_tracers_o2.csv")  
btm <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/btm.csv")  

d <- d[!is.na(d$OXYGEN_umolperkg), ]

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

long1 <- -55.5448
lat1 <- 53.6805
d$dist <- earthDist(long1, lat1, d$LONGITUDE, d$LATITUDE)


# get years
d$yr <- substr(d$EXPOCODE,4,7)
d$yr[d$EXPOCODE == " HUD2016006"] <- 2016  
d$yr[d$EXPOCODE == " AMU2019001"] <- 2019  
d$yr[d$EXPOCODE == " AMU2019001"] <- 2019  
d$yr[d$EXPOCODE == "AT4805"] <- 2022  
d$yr <- as.numeric(d$yr)



#quick qa/qc check by year
flags <- c(516052,515753,515605,516470,516091,515743,515663,516358,516290,515776,
           506325,505781,505770,505716,505633,499456,499436,499388,499388,499373,
           499350,499318,499202,498954,498947,498909,498829,492854,476824,476786,
           476671,438002,437779,437659,433655,407942,408200,438043,437756,437757,
           498922,437946)   

ind <- which(d$SAMPNO %in% flags)  
d <- d[-ind,] 
ind2 <- which(d$STNNBR==201)
d <- d[-ind2,] 
ind4 <- which(d$STNNBR==14)
d <- d[-ind4,] 
ind3 <- which(d$OXYGEN_mlperl<2)
d <- d[-ind3,]

# s <- d[d$yr==2023,]
# ustn <- as.numeric(unique(s$STNNBR))
# i <- ustn[4]
# for (i in ustn){
#   e <- s[s$STNNBR==i,]
#   do <- e$OXYGEN_umolperkg
#   sal <- e$CTDSAL
#   temp <- e$CTDTMP
#   p <- e$CTDPRS
#   stn <- unique(e$STNNBR)
#   par(mfrow = c(1, 3))
#   plot(do,-p,pch = 19, main = stn)
#   plot(sal,-p,pch = 19)
#   plot(temp,-p,pch = 19)
# }

# f <- s[s$STNNBR==98, ]


years <- sort(unique(d$yr))
years_to_plot <- years[1:min(9, length(years))]  

# section plots... 
# Save to a PNG 
png("output/do_lab_meas_2018.png", width = 1800, height = 1800, res = 200)

# Set layout and margins for 3x3
# par(mfrow = c(3, 3))
# par(cex = 0.6, mar = c(2, 2, 2, 1), oma = c(4, 4, 4, 2), tcl = -0.25, mgp = c(2, 0.6, 0))

year <- years_to_plot[3]  # test loop

for (year in years_to_plot) {
  d2 <- d[d$yr == year, ]
  if (nrow(d) == 0) next
  
  dist <- d2$dist
  pres <- -d2$CTDPRS
  z <- as.numeric(d2$OXYGEN_umolperkg)
  
  fld <- with(d2, interp(x = dist, y = pres, z = z, duplicate = "strip"))
  
  
  
  image.plot(x = fld$x, y = fld$y, z = fld$z,
             xlim = c(0, 900), ylim = c(-3700, -10), zlim = c(250,325),
             xlab = "Distance (km)", ylab = "Pressure (dbar)", main = paste(as.character(year), "DO (umol kg-1)"),
             # xlab = "", ylab = "", main = as.character(year),
             legend.mar = 3, legend.width = 1.2, legend.only = FALSE,
             cex.main = 2.5, cex.lab = 2, cex.axis = 2,
             col = viridis::viridis(100, option = "D")
             
  )
  
  
  contour(x = fld$x, y = fld$y, z = fld$z,
          xlim = c(0, 900), ylim = c(-3700, 0),
          nlevels = 15, add = TRUE, drawlabels = FALSE)
  
  points(dist, -d2$CTDPRS, pch = 20, cex = 0.5, col = "black")
  polygon(btm$dist, -btm$btm_S, col = "grey")
}

# Global labels
mtext("Distance (km)", side = 1, outer = TRUE, line = 2.5)
mtext("Pressure (dbar)", side = 2, outer = TRUE, line = 2.5)
mtext("Dissolved Oxygen Measurements (umol kg-1)", side = 3, outer = TRUE, line = 1.5, cex = 1.2)

# Finish PNG
dev.off()



