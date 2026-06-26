rm(list = ls())
setwd("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning")
dir()
s <- read.csv("Raimondi_MLD_S2.csv" ,header =  F, stringsAsFactors = F)   #read in data
head(s)  

# par(mfrow = c(3,1))# mar = c(5, 2, 3, 1 ))


plot(s$V1,s$V2, type = "b", pch = 19)#


plot(s$V1,s$V2, type = "b", pch = 19, xlim = c(2015, 2025))
