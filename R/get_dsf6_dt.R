# ==========================================================
# Function: get_df12_dt.R()
# Project: ttd
# Purpose: Calculate f12 first derivative of the atmospheric input function 
# ==========================================================

get_df12_dt <- function(atm_f12){
  atm_f12$yr <- floor(atm_f12$YEAR)
  annual <- aggregate(CFC12 ~ yr,
                      data = atm_f12,
                      FUN = mean)
  annual$dc_dt <- c(NA, diff(annual$CFC12) / diff(annual$yr))
  
  annual
}