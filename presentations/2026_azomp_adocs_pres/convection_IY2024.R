rm(list = ls())
library(oce)

d <- read.csv("C:/Users/childsd/repospace/ttd/data/mld_Fig4_Yashayaev2024.csv",header =  T, stringsAsFactors = F)   #read in data
head(d)  

guess <- tail(d,2)


plot(d$x, d$y, pch = 8, type = "both", ylab = "Convection Depth (m)", xlab = "Year", col = "navyblue")
points(guess$x,guess$y, col = "red", pch = 19)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
