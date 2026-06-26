rm(list = ls())
# Load necessary libraries
library(tidyverse)

# Read data (you may need to update the file path)
df <- read.csv("E:/A_docs/2025/2025_labSea/ttd_reading/ttd_learning/profile_data.csv")

# Atmospheric SF6 history (1970 to 2025, simplified example)
# years <- 1970:2025
# sf6_atm <- c(rep(0, 5), seq(0.01, 2.5, length.out = length(years) - 5))

sf6_atm <- read.csv("E:/A_docs/2025/2025_labSea/ttd_reading/ttd_learning/atmos_data_sf6_comb.csv")
sf6_interp <- approxfun(sf6_atm$Year, sf6_atm$SF6NH)


# Sampling year
sampling_year <- 2025

# Inverse Gaussian Transit Time Distribution
G_invgauss <- function(tau, Gamma, Delta) {
  (1 / sqrt(4 * pi * Delta^2 * tau^3)) * exp(-(tau - Gamma)^2 / (4 * Delta^2 * tau))
}

# Modeled SF6 given Gamma and Delta
model_sf6 <- function(Gamma, Delta, sampling_year) {
  tau_vals <- 1:50
  G_vals <- G_invgauss(tau_vals, Gamma, Delta)
  atm_vals <- sf6_interp(sampling_year - tau_vals)
  sum(G_vals * atm_vals) / sum(G_vals)
}

# Objective function: squared error between modeled and observed SF6
fit_ttd <- function(obs_sf6, sampling_year) {
  function(params) {
    Gamma <- params[1]
    Delta <- params[2]
    modeled <- model_sf6(Gamma, Delta, sampling_year)
    return((obs_sf6 - modeled)^2)
  }
}

# Fit TTD for all depths
results <- df %>%
  filter(!is.na(sf6_fmolperkg)) %>%
  mutate(
    Gamma = NA_real_,
    Delta = NA_real_,
    ratio = NA_real_
  )

for (i in 1:nrow(results)) {
  obs <- results$sf6_fmolperkg[i]
  if (!is.na(obs)) {
    opt <- optim(par = c(10, 3),
                 fn = fit_ttd(obs, sampling_year),
                 method = "L-BFGS-B",
                 lower = c(1, 0.5),
                 upper = c(100, 50))
    results$Gamma[i] <- opt$par[1]
    results$Delta[i] <- opt$par[2]
    results$ratio[i] <- opt$par[2] / opt$par[1]
  }
}

# Save the result
write.csv(results, "ttd_fit_results.csv", row.names = FALSE)

# Optional: Plot Gamma vs Depth
library(ggplot2)
ggplot(results, aes(x = Gamma, y = PrDM)) +
  geom_line() +
  scale_y_reverse() +
  labs(title = "Transit Time (Gamma) vs Depth", x = "Gamma (years)", y = "Pressure (dbar)")
