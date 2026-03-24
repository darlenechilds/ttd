

ttd_data <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/TTD_sf6_Glodap.csv")  # update with actual file path
ttd_data_2 <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/TTD_sf6_Glodap_2.csv")  # update with actual file path

plot_IG_ttd <- function(Gamma, Delta, t_max = 200, add = FALSE, col = "blue", ...) {
  t <- seq(0.1, t_max, by = 0.1)  # Avoid t=0
  G <- sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma * (t - Gamma)^2 / (4 * Delta^2 * t))
  
  
  if (!add) {
    plot(t, G, type = "l", col = col, lwd = 2,
         xlab = "Transit Time (years)",
         ylab = "G(t)",
         main = "Inverse Gaussian TTD", ...)
  } else {
    lines(t, G, col = col, lwd = 2, ...)
  }
}
# Typical ocean values
plot_IG_ttd(Gamma = 70, Delta = 70, col = "red")
plot_IG_ttd(Gamma = 50, Delta = 25, add = TRUE, col = "blue")
plot_IG_ttd(Gamma = 50, Delta = 45, add = TRUE, col = "darkgreen")
legend("topright", legend = c("Δ=10", "Δ=25", "Δ=45"),
       col = c("red", "blue", "darkgreen"), lty = 1, lwd = 2)
