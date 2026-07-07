# ==========================================================
# Function: get_noaa_atm_sf6()
# Project: ttd 
# ==========================================================
get_noaa_atm_sf6 <- function() {
  
  atm_history <- read.csv(
    "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0164584/CFC_ATM_Hist_2022/Tracer_atmospheric_histories_revised_2023_Table1.csv",
    header = TRUE)
  
  atm_history <- atm_history[-c(1:2), ]
  
  sf6_atm_monthly_means <- read.csv(
    "https://gml.noaa.gov/webdata/ccgg/trends/sf6/sf6_mm_gl.csv",
    comment.char = "#")
  
  sf6 <- atm_history[, c("YEAR", "SF6")]
  sf6$SF6 <- as.numeric(sf6$SF6)
  sf6 <- sf6[sf6$SF6 > 0 & sf6$YEAR < 1998, ]
  
  sf62 <- sf6_atm_monthly_means[, c("decimal", "average")]
  names(sf62) <- c("YEAR", "SF6")
  
  rbind(sf6, sf62)
}
