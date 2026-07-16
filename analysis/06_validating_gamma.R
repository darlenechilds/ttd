# ==========================================================
# Script: 06_validating_gamma.R
#
# Project:  ttd
# Purpose:  comparing and contrasting mean ages derived from sf6 adn F12
# 
# Notes:  for now using LRs time varying % saturation model based on f12 observed saturation, 
#     This is still exploratory...  manually visualizing the data
# ==========================================================
library(oce)

d_f12 <- read.csv("data/processed/f12_gamma.csv")
d <- read.csv("data/processed/sf6_gamma.csv")

#get ar7w line/ centre
d <- d[d$LATITUDE > 56 & d$LATITUDE < 60,]
d <- d[d$LONGITUDE > -54.5 & d$LONGITUDE < -45,]
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS,referencePressure = 0)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta,d$CTDPRS)


#get f12 for sf6 onwards
d <- merge(d,d_f12,by = "SAMPNO", all.x = T)
ind <- which(is.na(d$yr.y))
d <- d[-ind,]

#suspecting 2024 f12 data...
# d_yr <- which(d$yr.y==2024)
# d <- d[-d_yr,]

plot(d$Gamma.y,d$Gamma.x, ylim = c(0, 200), xlim = c(0, 200),
     xlab = "CFC-12 Age (years)",
     ylab = "SF6 Age (years)",
     pch = 16)
abline(0,1,col="red",lwd=2)

bias <- mean(d$Gamma.x - d$Gamma.y, na.rm = TRUE)
rmse <- sqrt(mean((d$Gamma.x - d$Gamma.y)^2, na.rm = TRUE))
r <- cor(d$Gamma.x,d$Gamma.y,use = "complete.obs")

cat("Bias =", round(bias,2), "years\n")
cat("RMSE =", round(rmse,2), "years\n")
cat("Correlation =", round(r,3), "\n")

hist(d$Gamma.x - d$Gamma.y)
plot(d$Gamma.x - d$Gamma.y, -d$CTDPRS.x)
plot(d$Gamma.x - d$Gamma.y, d$sigma2)

# look at each year
uyear <- unique(d$yr.y)
i <- uyear[3]
s <- NULL
for (i in uyear){
  e <- d[d$yr.y==i,]
  bias <- mean(e$Gamma.x - e$Gamma.y, na.rm = TRUE)
  rmse <- sqrt(mean((e$Gamma.x - e$Gamma.y)^2, na.rm = TRUE))
  r <- cor(e$Gamma.x,e$Gamma.y,use = "complete.obs")
  data <- c(i, bias, rmse, r)  
  s <- rbind(s, data)
}

s <- as.data.frame(s, row.names = F)
names(s) <- c("year","bias","rmse","r")
s

# look at each water mass
#get water masses
surf <- d[d$CTDPRS.x > 10 & d$CTDPRS.x < 200,]
bias <- mean(surf$Gamma.x - surf$Gamma.y, na.rm = TRUE)
rmse <- sqrt(mean((surf$Gamma.x - surf$Gamma.y)^2, na.rm = TRUE))
r <- cor(surf$Gamma.x,surf$Gamma.y,use = "complete.obs")

surf_s <- NULL
for (i in uyear){
  e <- surf[surf$yr.y==i,]
  bias <- mean(e$Gamma.x - e$Gamma.y, na.rm = TRUE)
  rmse <- sqrt(mean((e$Gamma.x - e$Gamma.y)^2, na.rm = TRUE))
  r <- cor(e$Gamma.x,e$Gamma.y,use = "complete.obs")
  data <- c(i, bias, rmse, r)  
  names(data) <- c("year","bias","rmse","r")
  surf_s <- as.data.frame(rbind(surf_s, data))
}

lsw_int <- d[d$CTDPRS.x > 200 & d$CTDPRS.x < 2000,]
lsw_int_s <- NULL
for (i in uyear){
  e <- lsw_int[lsw_int$yr.y==i,]
  bias <- mean(e$Gamma.x - e$Gamma.y, na.rm = TRUE)
  rmse <- sqrt(mean((e$Gamma.x - e$Gamma.y)^2, na.rm = TRUE))
  r <- cor(e$Gamma.x,e$Gamma.y,use = "complete.obs")
  data <- c(i, bias, rmse, r)  
  names(data) <- c("year","bias","rmse","r")
  lsw_int_s <- as.data.frame(rbind(lsw_int_s, data))
}

neadw <- d[d$sigma2 > 36.96 & d$sigma2 < 37.1,]
neadw_s <- NULL
for (i in uyear){
  e <- neadw[neadw$yr.y==i,]
  bias <- mean(e$Gamma.x - e$Gamma.y, na.rm = TRUE)
  rmse <- sqrt(mean((e$Gamma.x - e$Gamma.y)^2, na.rm = TRUE))
  r <- cor(e$Gamma.x,e$Gamma.y,use = "complete.obs")
  data <- c(i, bias, rmse, r)  
  names(data) <- c("year","bias","rmse","r")
  neadw_s <- as.data.frame(rbind(neadw_s, data))
}

# dsow <- d[d$CTDPRS > 3480,]

plot(surf_s$year,surf_s$bias, pch = 16, ylim = c(-50, 50), type = "b", ylab = "bias")
points(lsw_int_s$year,lsw_int_s$bias, pch = 16, col = "blue", type = "b")
points(neadw_s$year,neadw_s$bias, pch = 16, col = "darkgreen", type = "b")

plot(surf_s$year,surf_s$rmse, pch = 16, ylim = c(-25, 60), type = "b", ylab = "rmse")
points(lsw_int_s$year,lsw_int_s$rmse, pch = 16, col = "blue", type = "b")
points(neadw_s$year,neadw_s$rmse, pch = 16, col = "darkgreen", type = "b")

#######
i <- uyear[2]
for (i in uyear){
  e <- d[d$yr.y==i,]
  plot(e$Gamma.x,-e$CTDPRS.x, pch = 16, main = i, xlim = c(0,200))
  
}

