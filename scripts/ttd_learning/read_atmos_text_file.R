rm(list = ls())

# get text atmos data

setwd("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning")
dir()


d <- read.table("GML_NOAA_SF6_global_mean_rateofchange.txt" )
head(d)

write.csv(d,"GML_NOAA_SF6_global_mean_rateofchange.csv" )
