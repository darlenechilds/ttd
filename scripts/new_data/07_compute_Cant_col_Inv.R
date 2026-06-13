# Compute_Cant Column Inventories, smooth lat-deepth section bridded onto standard coordinates
rm(list = ls())

library(akima)
library(fields)
library(oce)

# all data
# d <- read.csv("data/processed/new/Cant.csv", stringsAsFactors = F)
#lsw
d <- read.csv("data/processed/new/Cant_lsw.csv", stringsAsFactors = F)
unique(d$year)

# as in raimondi get central lab sea, stn with btm depth of >3300
# d$stn_id <- paste0(d$EXPOCODE,d$STNNBR)
# stn_3300 <- d[d$CTDPRS>=3300,]
# stn_lswc <- unique(stn_3300$stn_id)
# d <- d[d$stn_id %in% stn_lswc, ]

d$rho <- swRho(d$CTDSAL,d$theta,d$CTDPRS)


s <- NULL
i <- 2016
for (i in unique(d$year)){
e <- d[d$year==i,]

# interpolate Cant profiles in lsw centre
fit <- Tps(x = cbind(e$LATITUDE, e$CTDPRS),Y = e$Cant)
grid <- expand.grid(lat = seq(56.1,60,by=0.05), depth = seq(0,3400,by=5))
grid$Cant_map <- predict(fit,cbind(grid$lat, grid$depth))
z.matrix <- matrix(grid$Cant_map,nrow = length(unique(grid$lat)),ncol = length(unique(grid$depth)))

# interpolate rho
fit_rho <- Tps(x = cbind(e$LATITUDE, e$CTDPRS),Y = e$rho)
grid$rho_map <- predict(fit_rho,cbind(grid$lat, grid$depth))  # grid$cant_map and grid$rho_map align point by point
rho.matrix <- matrix(grid$rho_map,nrow = length(unique(grid$lat)),ncol = length(unique(grid$depth)))


# filled.contour(
#   x = unique(grid$lat),
#   y = unique(grid$depth),
#   z = z.matrix,
#   ylim = rev(range(unique(grid$depth))),
#   xlab = "Latitude",
#   ylab = "Depth (m)",
#   main = paste("Mapped Cant (umol kg-1)",yr)
# )

Cant_mean <- colMeans(z.matrix, na.rm=TRUE)
rho_mean <- colMeans(rho.matrix, na.rm=TRUE)

dz <- 5
inventory <- sum(
  Cant_mean * rho_mean * dz,
  na.rm = TRUE
)

cant_inv <- c(i, inventory/1e6)
s <- rbind(s,cant_inv)
}

s


