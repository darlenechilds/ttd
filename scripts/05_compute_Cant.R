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
yr <- 2004
fn <- paste("data/processed/gamma_best_",yr,".csv", sep = "")
d <- read.csv(fn, stringsAsFactors = F)


# co2_cant_surf - input to the system
cO <- read.csv("data/processed/surf_pco2_history.csv")
cO_fun <- approxfun(cO$yr, cO$dic_cant, rule = 1) 

# have gamma
Gamma <- d$gamma_best
ratio <- 1.8
Delta <- ratio * Gamma

d$Cant <- sapply(1:length(Gamma), function(i) {
  compute_Cxt(
    t = yr,
    Gamma = Gamma[i],
    Delta = Delta[i],
    C0_fun = cO_fun
  )
})

#find lsw mean Cant
lsw_sigma <- data.frame(
  year = c(1986, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
           2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
           2011, 2012, 2013, 2014, 2015, 2016),
  sigma2 = c(36.910, 36.943, 36.952, 36.953, 36.942, 36.931, 36.913, 36.893,
             36.870, 36.875, 36.887, 36.885, 36.887, 36.881, 36.868, 36.864,
             36.842, 36.874, 36.860, 36.817, 36.823, 36.852, 36.850, 36.860,
             36.879, 36.896))

sigma_cut <- lsw_sigma$sigma2[lsw_sigma$year == yr]

# LSW
lsw <- d[d$year == yr &
           d$LATITUDE > 56 & d$LATITUDE < 59.1 &
           d$CTDPRS > 200 &
           d$sigma2 < sigma_cut, ]
mean(lsw$Cant)


# NEADW
neadw <- d[d$year == yr &
             d$LATITUDE > 56 & d$LATITUDE < 59.1 &
             d$sigma2 > 36.965 & d$sigma2 < 37.04, ]

mean(neadw$Cant)

write.csv(d,fn, row.names = F)
