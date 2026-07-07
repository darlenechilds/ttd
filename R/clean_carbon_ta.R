# ==========================================================
# Function: clean_carbon_ta()
# Project: ttd 
# ==========================================================
clean_carbon_ta<- function(d) {
  
  d <- d[c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
           "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY","SALNTY_FLAG_W",
           "ALKALI","ALKALI_FLAG_W")]
  numeric_cols <- c("STNNBR","LATITUDE", "LONGITUDE", "CTDPRS", "CTDTMP", "CTDSAL",
                    "SALNTY",  "ALKALI")
  d[numeric_cols] <- lapply(d[numeric_cols], as.numeric)
  d <- d[d$ALKALI_FLAG_W ==2 | d$ALKALI_FLAG_W==6, ]
  
  #remove do = 0, -999
  d$ALKALI[d$ALKALI == -999] <- NA
  d <- d[!is.na(d$ALKALI), ]
  
  return(d)
}
