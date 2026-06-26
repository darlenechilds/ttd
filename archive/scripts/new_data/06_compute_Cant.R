# Compute_Cant
rm(list = ls())


# functions 
# ttd function
ttd <- function(t, Gamma, Delta) {
  G <- sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma * (t - Gamma)^2 / (4 * Delta^2 * t))
  G[t <= 0] <- 0
  return(G)
}
compute_Cxt <- function(t_today, Gamma, Delta, C0_fun) {
  tau  <- seq(0.1, 200, by = 0.5)
  dtau <- 0.5
  # Transit time distribution
  G_tau <- ttd(tau, Gamma, Delta)
  # Normalize so integral = 1
  G_tau <- G_tau / sum(G_tau * dtau)
  # Surface boundary condition evaluated backward in time
  C0_vals <- C0_fun(t_today - tau)
  C0_vals[is.na(C0_vals)] <- 0
  # Convolution
  C_xt <- sum(C0_vals * G_tau * dtau)
  return(C_xt)
}


# input found gamma
d <- read.csv("data/processed/new/gamma_best.csv", stringsAsFactors = F)

mld <- read.csv("data/raw/mld_Fig4_Yashayaev2024.csv")
mld$year <- floor(mld$x)

# co2_cant_surf - input to the system
cO <- read.csv("data/processed/surf_pco2_history.csv")
cO_fun <- approxfun(cO$yr, cO$dic_cant, rule = 1) 

d_Cant <- NULL
yrs <- 2015:2025
j <- 2018
out <- vector("list", length(yrs))

for (j in seq_along(yrs)) {
  
  yr <- yrs[j]
  
  e <- d[d$year == yr, ]
  
  Gamma <- e$gamma_best
  Delta <- 1.8 * Gamma
  
  e$Cant <- sapply(seq_along(Gamma), function(i) {
    
    if (is.na(Gamma[i])) return(NA)
    
    compute_Cxt(
      t = yr,
      Gamma = Gamma[i],
      Delta = Delta[i],
      C0_fun = cO_fun
    )
    
  })
  
  out[[j]] <- e
}

d_Cant <- do.call(rbind, out)
d <- d_Cant

#find lsw mean Cant for last 10 years
lsw <- NULL
i <- 2015
for (i in 2015:2025){
  mld_cut <- mld$y[mld$year == i]
  # sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == i]
  
  e <- d[d$year == i &
             d$LATITUDE > 56 & d$LATITUDE < 59.1 &
             d$CTDPRS > 200 &
             d$CTDPRS < mld_cut, ]
  lsw <- rbind.data.frame(lsw,e)
}
unique(lsw$year)
Cant_means <- aggregate(Cant ~ year, data = lsw, mean, na.rm = TRUE)
plot(Cant_means$year,Cant_means$Cant)
head(lsw)


write.csv(d,"data/processed/new/Cant.csv", row.names = F)
write.csv(lsw,"data/processed/new/Cant_lsw.csv", row.names = F)
  