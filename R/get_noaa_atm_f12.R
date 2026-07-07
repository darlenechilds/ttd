# ==========================================================
# Function: get_noaa_atm_f12()
# Project: ttd 
# ==========================================================
get_noaa_atm_f12 <- function() {
  
  atm_history <- read.csv(
    "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0164584/CFC_ATM_Hist_2022/Tracer_atmospheric_histories_revised_2023_Table1.csv",
    header = TRUE)
  
  atm_history <- atm_history[-c(1:2), ]
  
  f12_global <- read.csv("https://gml.noaa.gov/aftp/data/hats/cfcs/cfc12/combined/HATS_global_F12.txt",
                         comment.char = "#",header = TRUE,sep = "")
  
  f12 <- atm_history[,c("YEAR","CFC12")]
  f12$CFC12 <- as.numeric(f12$CFC12)
  f12 <- f12[f12$CFC12>0,]
  f12 <- f12[f12$YEAR<1978,]
  f12_global$d_year <- f12_global$HATS_F12_YYYY+(f12_global$HATS_F12_MM/12)
  f12_2 <- f12_global[,c("d_year","HATS_NH_F12")]
  colnames(f12_2) <- c("YEAR","CFC12")
  f12 <- rbind.data.frame(f12,f12_2)
  
}
