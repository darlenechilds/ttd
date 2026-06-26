rm(list = ls())
# Define the TTD function (Inverse Gaussian)
ttd <- function(t, Gamma, Delta) {
  # ifelse(t <= 0, 0,
  sqrt(Gamma^3 / (4 * pi * Delta^2 * t^3)) *
    exp(-Gamma* (t-Gamma)^2 / (4 * Delta^2 * t))
}

# Define parameters
Gamma <- 10
rel_widths <- c(0.5, 1.0, 1.5)  # Δ/Γ values
t_vals <- seq(0.1, 30, by = 0.1)  # Avoid t = 0 to prevent division by zero

# Set up plot
plot(NULL, xlim = c(0, 30), ylim = c(0, 0.3),
     xlab = "Transit Time (years)",
     ylab = "Probability Density",
     main = "Inverse Gaussian TTDs (Smith et al. 2022)")

# Plot curves for each Δ/Γ
colors <- c("blue", "green", "red")
for (i in seq_along(rel_widths)) {
  Delta <- rel_widths[i] * Gamma
  lines(t_vals, ttd(t_vals, Gamma, Delta), col = colors[i], lwd = 2)
}

# Add legend
legend("topright", legend = paste0("Δ/Γ = ", rel_widths),
       col = colors, lwd = 2)
