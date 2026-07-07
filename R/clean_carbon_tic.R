# ==========================================================
# Function: clean_carbon_tic()
# Project: ttd 
# ==========================================================
clean_carbon_tic<- function(d) {
  
  d <- d[c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
           "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY","SALNTY_FLAG_W",
           "TCARBN","TCARBN_FLAG_W")]
  numeric_cols <- c("STNNBR","LATITUDE", "LONGITUDE", "CTDPRS", "CTDTMP", "CTDSAL",
                    "SALNTY",  "TCARBN")
  d[numeric_cols] <- lapply(d[numeric_cols], as.numeric)
  d <- d[d$TCARBN_FLAG_W ==2 | d$TCARBN_FLAG_W==6, ]
  
  #remove do = 0, -999
  d$TCARBN[d$TCARBN == -999] <- NA
  d <- d[!is.na(d$TCARBN), ]
  
  return(d)
}
