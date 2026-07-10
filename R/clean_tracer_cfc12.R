# ==========================================================
# Function: clean_tracer_cfc12()
# Project: ttd 
# ==========================================================
clean_tracer_cfc12<- function(d) {
  
  d <- d[c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
           "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY","SALNTY_FLAG_W",
           "CFC12","CFC12_FLAG_W")]
  numeric_cols <- c("STNNBR","LATITUDE", "LONGITUDE", "CTDPRS", "CTDTMP", "CTDSAL",
                    "SALNTY",  "CFC12")
  d[numeric_cols] <- lapply(d[numeric_cols], as.numeric)
  d_2025 <- d[d$EXPOCODE=="35A320250427",]
  d <- d[d$CFC12_FLAG_W ==2 | d$CFC12_FLAG_W==6, ]
  
  #ocads have not flagged 2025 yet so it needs to be put back into the dataframe
  d <- rbind(d,d_2025)
    
  #remove do = 0, -999
  d$CFC12[d$CFC12 == -999] <- NA
  d <- d[!is.na(d$CFC12), ]
  
  #found while computing gamma
  d <- d[!d$SAMPNO %in% c(437855, 437849, 437854, 437851, 437853, 437852), ]
  return(d)
}
