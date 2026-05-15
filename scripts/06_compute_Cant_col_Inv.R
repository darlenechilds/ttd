# Compute_Cant Column Inventories, smooth lat-deepth section bridded onto standard coordinates
rm(list = ls())

library(akima)
library(fields)
library(oce)

yr <- 2004
fn <- paste("data/processed/gamma_best_",yr,".csv", sep = "")
d <- read.csv(fn, stringsAsFactors = F)

# as in raimondi get central lab sea, stn with btm depth of >3300
stn_3300 <- d[d$CTDPRS>=3300,]
stn_lswc <- unique(stn_3300$STNNBR)
d <- d[d$STNNBR %in% stn_lswc, ]
d$rho <- swRho(d$CTDSAL,d$theta,d$CTDPRS)

# interpolate Cant profiles in lsw centre
fit <- Tps(x = cbind(d$LATITUDE, d$CTDPRS),Y = d$Cant)
grid <- expand.grid(lat = seq(56.1,60,by=0.05), depth = seq(0,3400,by=5))
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


filled.contour(
  x = unique(grid$lat),
  y = unique(grid$depth),
  
  z = rho.matrix,
  
  ylim = rev(range(unique(grid$depth))),
  
  xlab = "Latitude",
  ylab = "Depth (m)",
  main = "Mapped rho"
)


Cant_mean <- colMeans(z.matrix, na.rm=TRUE)
rho_mean  <- colMeans(rho.matrix, na.rm=TRUE)

inventory <- sum(
  Cant_mean * rho_mean * dz,
  na.rm = TRUE
)

inventory/1e6

#plot mean Cant profile
depths <- sort(unique(grid$depth))

plot(mean_profile,depths,  type = "l",  ylim = rev(range(depths)),  xlab = "Cant",  ylab = "Depth")

