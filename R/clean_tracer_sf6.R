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
  d <- d[d$SF6_FLAG_W ==2 | d$SF6_FLAG_W==6, ]
  
  #remove do = 0, -999
  d$SF6[d$SF6 == -999] <- NA
  d <- d[!is.na(d$SF6), ]
  
  return(d)
}
