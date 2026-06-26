# try and see if we can determine a difference using the means

rm(list = ls())
library(dplyr)
library(geosphere)
library(oce)

#ref data
rd <- read.csv("data/ref_means.csv")
rd <- rd[!duplicated(rd$mean_sf6),]

# get test cruise
d <- read.csv("data/d_means.csv")
