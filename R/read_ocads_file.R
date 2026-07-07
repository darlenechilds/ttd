# ==========================================================
# Function: read_ocads_file()
# project: ttd
# Purpose:
# Read all OCADS hydrographic files, standardize variable
# names, and return one combined data frame.
#
# Returns:
#   tibble containing all OCADS observations
# ==========================================================
read_ocads_file <- function(url) {
  
  # 1. read file
  d <- read_ocads(url)
  
  # 2. force everything to character (safe for messy OCADS)
  # d[] <- lapply(d, as.character)
  
  # 3. remove obvious junk rows
  d <- d[d$EXPOCODE != "END_DATA" &
           !is.na(d$CTDPRS) &
           d$CTDPRS != "DBARS",]
  # 3a. remove unit row
  d <- d[-1,]
  
  #4. rename CFC12
  
  if ("CFC-12" %in% names(d)) {
    names(d)[names(d) == "CFC-12"] <- "CFC12"}
  if ("CFC-12_FLAG_W" %in% names(d)) {
    names(d)[names(d) == "CFC-12_FLAG_W"] <- "CFC12_FLAG_W"}
  if ("CFC.12" %in% names(d)) {
    names(d)[names(d) == "CFC.12"] <- "CFC12"}
  if ("CFC.12_FLAG_W" %in% names(d)) {
    names(d)[names(d) == "CFC.12_FLAG_W"] <- "CFC12_FLAG_W"}
  
  #4a. rename lat/long  (2022 onward uses "btl_log/lat")
  if ("BTL_LAT" %in% names(d)) {
    names(d)[names(d) == "BTL_LAT"] <- "LATITUDE"}
  if ("BTL_LON" %in% names(d)) {
    names(d)[names(d) == "BTL_LON"] <- "LONGITUDE"}
  
  #5 add missing variables
  vars <- c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
            "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL",
            "SALNTY","SALNTY_FLAG_W",
            "CFC12","CFC12_FLAG_W","SF6","SF6_FLAG_W",
            "CTDOXY","CTDOXY_FLAG_W","OXYGEN","OXYGEN_FLAG_W",
            "TCARBN","TCARBN_FLAG_W","ALKALI","ALKALI_FLAG_W")
  
  missing <- setdiff(vars, names(d))
  for(i in missing){d[[i]] <- NA}
  #order columns
  d <- d[vars]
  
  
  # 7. return clean dataframe
  return(d)
}
