# Compute_Cant Column Inventories, smooth lat-depth section gridded onto standard coordinates
rm(list = ls())

library(akima)
library(fields)
library(oce)

#lsw sigma2 limit from raimondi et al., 2021, table S3
lsw_sigma <- data.frame(
  year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
           2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
           2011, 2012, 2013, 2014, 2015, 2016),
  sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
             36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
             36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
             36.879, 36.896))

yr <- 2004

sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == yr]

fn <- paste("archive/data/processed/gamma_best_",yr,".csv", sep = "")
d <- read.csv(fn, stringsAsFactors = F)

# as in raimondi get central lab sea, stn with btm depth of >3300
stn_3300 <- d[d$CTDPRS>=3300,]
stn_lswc <- unique(stn_3300$STNNBR)
d <- d[d$STNNBR %in% stn_lswc, ]
d$rho <- swRho(d$CTDSAL,d$theta,d$CTDPRS)
# d <- d[d$sigma2 < sigma_cut, ]

# interpolate Cant profiles in lsw centre
fit <- Tps(x = cbind(d$LATITUDE, d$CTDPRS),Y = d$Cant)
grid <- expand.grid(lat = seq(56.1,60,by=0.05), depth = seq(0,1300,by=5))
grid$Cant_map <- predict(fit,cbind(grid$lat, grid$depth))
z.matrix <- matrix(grid$Cant_map,nrow = length(unique(grid$lat)),ncol = length(unique(grid$depth)))

# interpolate rho
fit_rho <- Tps(x = cbind(d$LATITUDE, d$CTDPRS),Y = d$rho)
grid$rho_map <- predict(fit_rho,cbind(grid$lat, grid$depth))  # grid$cant_map and grid$rho_map align point by point
rho.matrix <- matrix(grid$rho_map,nrow = length(unique(grid$lat)),ncol = length(unique(grid$depth)))



filled.contour(
  x = unique(grid$lat),
  y = unique(grid$depth),
  
  z = z.matrix,
  
  ylim = rev(range(unique(grid$depth))),
  
  xlab = "Latitude",
  ylab = "Depth (m)",
  main = "Mapped Cant"
)


# filled.contour(
#   x = unique(grid$lat),
#   y = unique(grid$depth),
#   
#   z = rho.matrix,
#   
#   ylim = rev(range(unique(grid$depth))),
#   
#   xlab = "Latitude",
#   ylab = "Depth (m)",
#   main = "Mapped rho"
# )


Cant_mean <- colMeans(z.matrix, na.rm=TRUE)
rho_mean  <- colMeans(rho.matrix, na.rm=TRUE)
dz <- 10

inventory <- sum(Cant_mean * rho_mean * dz,  na.rm = TRUE)

inventory/1e6

#plot mean Cant profile
depths <- sort(unique(grid$depth))

plot(mean_profile,depths,  type = "l",  
     ylim = rev(range(depths)),  xlab = "Cant",  ylab = "Depth")

