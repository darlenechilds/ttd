# ==========================================================
# Function: compute_inventory_year()
# Project: ttd 
# compute inventories for observations (sf6, f12, o2, etc.)
#   for each year
# ==========================================================

compute_inventory_year <- function(s, year, tracer) {
  
  d <- s[s$yr == year, ]
  
  # interpolate tracer
  fit <- Tps(x = cbind(d$LATITUDE, d$CTDPRS),Y = d[[tracer]])
  grid <- expand.grid(lat = seq(56.1, 60, by = 0.05),depth = seq(0, 3400, by = 5))
  grid$tracer_map <- predict(fit,cbind(grid$lat, grid$depth))
  tracer.matrix <- matrix(grid$tracer_map,nrow = length(unique(grid$lat)),ncol = length(unique(grid$depth)))
  
  # interpolate density
  fit_rho <- Tps(x = cbind(d$LATITUDE, d$CTDPRS),Y = d$rho)
  grid$rho_map <- predict(fit_rho,cbind(grid$lat, grid$depth))
  rho.matrix <- matrix(grid$rho_map,nrow = length(unique(grid$lat)),ncol = length(unique(grid$depth)))
  
  # average across latitude
  tracer_mean <- colMeans(tracer.matrix, na.rm = TRUE)
  rho_mean <- colMeans(rho.matrix, na.rm = TRUE)
  
  dz <- 5
  
  inv <- sum(tracer_mean * rho_mean * dz, na.rm = TRUE)
  
  conversion <- switch(tracer,
                       CFC12 = 1e-12,
                       SF6   = 1e-15,
                       OXYGEN   = 1e-6,
                       stop("Unknown tracer"))
  
  inv <- inv * conversion
  
  
  
  return(inv)
}
