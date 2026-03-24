rm(list = ls())

ttd_data <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/TTD_sf6_Glodap_labdata.csv")  # update with actual file path
head(ttd_data)

ttd_data <- ttd_data[which(ttd_data$year==2020),]
plot(ttd_data$longitude,ttd_data$latitude)
ustn <- unique(ttd_data$event)
i <- ustn[3]
for(i in ustn){
  e <- ttd_data[which(ttd_data$event==i),]
  sf6 <- e$sf6_obs
  sf6_model <- e$sf6_model
  gamma <- e$Gamma
  delta <- e$Delta
  error <- e$error
  p <- e$PrDM
  stn <- unique(e$STNNBR)
  
  par(mfrow = c(1,4), mar = c(5, 2, 3, 1 ))
  plot(sf6_model, -p, xlim = c(0, 7),ylim = c(-3700,0), pch = 16, main = stn)
  points(sf6, -p, pch = 16, col = "red")
  plot(gamma, -p, xlim = c(13,65),ylim = c(-3700,0), pch = 16)
  plot(delta, -p, xlim = c(0, 20),ylim = c(-3700,0), pch = 16)
  plot(error, -p, xlim = c(2.236764e-22, 4.730634e+00),ylim = c(-3700,0), pch = 16)
}

