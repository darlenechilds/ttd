# ==========================================================
# Function: clean_tracer_sf6()
# Project: ttd 
# ==========================================================
clean_tracer_sf6<- function(d) {
  
  d <- d[c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
           "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY","SALNTY_FLAG_W",
           "SF6","SF6_FLAG_W")]
  numeric_cols <- c("STNNBR","LATITUDE", "LONGITUDE", "CTDPRS", "CTDTMP", "CTDSAL",
                    "SALNTY",  "SF6")
  d[numeric_cols] <- lapply(d[numeric_cols], as.numeric)
  d_2025 <- d[d$EXPOCODE=="35A320250427",]
  d <- d[d$SF6_FLAG_W ==2 | d$SF6_FLAG_W==6, ]
  
  #ocads have not flagged 2025 yet so it needs to be put back into the dataframe
  d <- rbind(d,d_2025)
  
  #remove do = 0, -999
  d$SF6[d$SF6 == -999] <- NA
  d <- d[!is.na(d$SF6), ]
  
  #found bad data while qcing
  d <- d[!d$SAMPNO==437830,]
  #found while computing gamma
  d <- d[!d$SAMPNO %in% c(437855, 437849, 437854, 437850, 437851, 437853, 437852), ]
  return(d)
}
