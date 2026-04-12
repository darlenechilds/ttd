# try to understand mld computation (from Yash. and Loder 2016) 
rm(list = ls())
library(oce)

# #bottle data
# d <- read.csv("data/OCADS_tracers_o2.csv")    #OCADS data
# 
# #filter out good data, 2 = good, 6 = mean of duplicates
# d <- d[d$cfc12_flag==2 | d$cfc12_flag ==6,]
# 
# #add year
# d$year <- substr(d$DATE,1,4)
# 
# # filter out a couple of profiles..
# d2 <- d[d$year==2010 | d$year==2018,]
# d2 <- d2[!is.na(d2$LONGITUDE) & d2$LONGITUDE > -52 & d2$LONGITUDE < -51, ]
# #check
# plot(d2$LONGITUDE,d2$LATITUDE)
# 
# #compute density, vertical gradient 
# d2$rho <- swRho(d2$SALNTY,d2$CTDTMP,d2$CTDPRS)
# 
# unique(d2$STNNBR)
# #get one profile only to really see the math
# 
# d3 <- d2[d2$STNNBR==100,]
# 
# d_sigma <- diff(d3$rho, lag = 1, differences = 1)
# d_pres <- diff(d3$CTDPRS, lag = 1, differences = 1)
# grad_sigma <- d_sigma/d_pres
# d3 <- d3[-1,]
# plot(grad_sigma,-d3$CTDPRS,main = unique(d3$year))
# 
#ctd data
library(oce)
files <- list.files(
  path = "R:/Science/BIODataSvc/ARC/Archive/ctd/2018/",
  pattern = "^CTD_HUD2018008_.*_DN\\.ODF$",
  full.names = TRUE
)

ctd_list <- lapply(files, read.oce)

lat <- sapply(ctd_list, function(x) x[["latitude"]])
lon <- sapply(ctd_list, function(x) x[["longitude"]])

plot(lon, lat,
     xlab = "Longitude", ylab = "Latitude",
     pch = 19)

#subset centre labrador sea, lsw_c
lsw_c_idx <- lat > 56 & lat < 59 & lon > -52 & lon < -48

lsw_c <- as.section(ctd_list[lsw_c_idx])

lat_sub <- lsw_c[["latitude"]]
lon_sub <- lsw_c[["longitude"]]

plot(lon_sub, lat_sub,
     xlab = "Longitude", ylab = "Latitude",
     main = "Subset: LSW Center",
     pch = 19)

#compute mld for one profile, 2 methods, 75% & 0.03 surf
lsw_list <- ctd_list[lsw_c_idx]
ctd <- lsw_list[[2]]  

p <- ctd[["pressure"]]
t <- ctd[["temperature"]]
s <- ctd[["salinity"]]
t_pot <- gsw_pt0_from_t(s,t,p)

#density plot
sigma0 <- gsw_sigma0(s, t)
plot(sigma0, p, type = "l",
     ylim = rev(range(p)),
     xlab = "Sigma0 (kg/m^3 - 1000)",
     ylab = "Pressure")

#vert. density gradient plot
d_sigma <- c(NA,diff(sigma0, lag = 1, differences = 1))
d_pres <- c(NA,diff(p, lag = 1, differences = 1))
grad_sigma <- d_sigma/d_pres
plot(grad_sigma,-p)


#compute mld, 0.03 kg/m^3 - fixed phys. density change fr. surf. 

sigma0_0 <- sigma0[1]
threshold <- sigma0_0 + 0.03
mld_idx <- which(sigma0 > threshold)[1]
mld <- p[mld_idx]
mld


start_idx <- which(p > 300)[1]   # skip top xxx dbar
sigma_ref <- sigma0[start_idx]
threshold <- sigma_ref + 0.03
mld_idx <- which(sigma0 > threshold & seq_along(p) > start_idx)[1]
mld <- p[mld_idx]
mld

#compute mld, 75th %
#compute vert. density gradient
grad_sigma <- diff(sigma0) / diff(p)
threshold <- quantile(abs(grad_sigma), 0.75, na.rm = TRUE)
mld_idx <- which(abs(grad_sigma) > threshold)[1]
mld <- p[mld_idx + 1]
mld
