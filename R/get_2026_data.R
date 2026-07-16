# ==========================================================
# Function: get_2026_data.R
#
# Project:  ttd
# Purpose: get recent unsubmitted 2026 data from SRC
# 
# ==========================================================
get_2026_data <- function(tracer){
  
  d <- read.csv("R:/Science/BIODataSvc/SRC/2020s/2026/JC291/BioChem/tracers/data/tracers.csv",stringsAsFactors = FALSE)
  
  if (tracer == "f12") {
    
    s <- d[c("cruise_number","event","event","sample_id","date","time",
             "eventLatitude","eventLongitude","PrDM","T090C","Sal00","Sal00","Sal00",
             "f12_pmolperkg","f12_pmolperkg")]
    
    names(s) <- c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
                  "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY",
                  "SALNTY_FLAG_W","CFC12","CFC12_FLAG_W")
    
    s$STNNBR <- NA
    s$SALNTY <- NA
    s$SALNTY_FLAG_W <- NA
    s$CFC12_FLAG_W <- NA
    s$DATE <- trimws(s$DATE)
    s$DATE <- format(as.Date(s$DATE, format = "%b %d %Y"), "%Y%m%d")
    
  } else if (tracer == "sf6") {
    
    s <- d[c("cruise_number","event","event","sample_id","date","time",
             "eventLatitude","eventLongitude","PrDM","T090C","Sal00","Sal00","Sal00",
             "sf6_fmolperkg","sf6_fmolperkg")]
    
    names(s) <- c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
                  "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL","SALNTY",
                  "SALNTY_FLAG_W","SF6","SF6_FLAG_W")
    
    s$STNNBR <- NA
    s$SALNTY <- NA
    s$SALNTY_FLAG_W <- NA
    s$SF6_FLAG_W <- NA
    s$DATE <- trimws(s$DATE)
    s$DATE <- format(as.Date(s$DATE, format = "%b %d %Y"), "%Y%m%d")
  }
  
  return(s)
}

