# ==========================================================
# Function: ttd()
# Project: ttd 
# Age distrubution (Green's function) that describes teh 
# propagation of surface boundary conditions into teh ocean
# interior - defined by an Inverse Guassian function 
# characterized by a mean (gamma) and a width (delta).
# ==========================================================
ttd <- function(t, Gamma, Delta) {
  G <- sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma * (t - Gamma)^2 / (4 * Delta^2 * t))
  G[t <= 0] <- 0
  return(G)
}