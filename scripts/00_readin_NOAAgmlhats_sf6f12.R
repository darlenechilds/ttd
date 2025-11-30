#read in data from NOAA GML website; https://gml.noaa.gov/ccgg/trends_sf6/
rm(list = ls())
library(dplyr)

sf6_atm_monthly_means <- read.csv("https://gml.noaa.gov/webdata/ccgg/trends/sf6/sf6_mm_gl.csv")

#greenland
f12 <- read.csv("https://gml.noaa.gov/aftp/data/hats/cfcs/cfc12/insituGCs/CATS/monthly/sum_F12_MM.dat")
#Alaska - Barrow
f12 <- read.csv("https://gml.noaa.gov/aftp/data/hats/cfcs/cfc12/insituGCs/CATS/monthly/brw_F12_MM.dat")




fn <- urls[5]  # file working on - sadly, still manual b/c cant work around cfc header mismatch

# find which line the EXPOCODE starts adn extract data
lines <- readLines(fn)
header <- grep("^EXPO", lines, value = TRUE)
header <- strsplit(header, ",")[[1]]
skip_no <- grep("^EXPO", lines, value = F)

#read in ocads
d <- read.csv(fn, skip = skip_no, header = F)
units <- d[1,]  #double check units
d <- d[-1,]
colnames(d) <- header
