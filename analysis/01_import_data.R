# ==========================================================
# Script: 01_import_data.R
#
# Project:  ttd
# Purpose:
# Import all raw datasets required for the TTD analysis.
#
# Inputs:
#   data/raw/
#
# Outputs:
#   data/processed/
# ==========================================================

#libaries
library(oce)

#functions
source("R/read_ocads.r")
source("R/read_ocads_file.r")
source("R/clean_o2.r")
source("R/clean_tracer_cfc12.r")
source("R/clean_tracer_sf6.r")
source("R/clean_carbon_ta.r")
source("R/clean_carbon_tic.r")
source("R/get_noaa_atm_sf6.r")
source("R/get_noaa_atm_f12.r")


#ocads data
ocadsFile <- "data/processed/ocads.rds"
if(file.exists(ocadsFile)){
  d <- readRDS("data/processed/ocads.rds")  # load ocads file if already compiled
} else {
d <- read.csv("data/raw/ocads_urls.csv")
urls <- d$url
ocads_list <- lapply(urls, read_ocads_file)  # read every OCADS file
ocads <- do.call(rbind, ocads_list)  # combine into one dataframe
saveRDS(ocads, "data/processed/ocads.rds")  #Save ocads data
d <- readRDS("data/processed/ocads.rds")  
}

#get/clean O2 data
o2 <- clean_o2(d)

#get/clean tracer data
f12 <- clean_tracer_cfc12(d)
sf6 <- clean_tracer_sf6(d)

#get/clean carbon data
ta <- clean_carbon_ta(d)
tic <- clean_carbon_tic(d)

#atmospheric histories
sf6_atm <- get_noaa_atm_sf6()
f12_atm <- get_noaa_atm_f12()
pco2_atm <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.csv", stringsAsFactors = F, skip = 37)

#save datasets
write.csv(o2,"data/processed/ocads_clean_o2.csv", row.names = F)
write.csv(f12,"data/processed/ocads_clean_f12.csv", row.names = F)
write.csv(sf6,"data/processed/ocads_clean_sf6.csv", row.names = F)
write.csv(ta,"data/processed/ocads_clean_ta.csv", row.names = F)
write.csv(tic,"data/processed/ocads_clean_tic.csv", row.names = F)
write.csv(f12_atm,"data/processed/noaa_f12_atm.csv", row.names = F)
write.csv(sf6_atm,"data/processed/noaa_sf6_atm.csv", row.names = F)
write.csv(pco2_atm,"data/processed/noaa_pco2_atm.csv", row.names = F)
