# merge tracer data with oxygen data (UNSUB)
rm(list = ls())
library(dplyr)
library(oce)

#load data
d <- read.csv("data/UNSUB_tracers.csv")   #read in tracer data
o2 <- read.csv("data/UNSUB_oxygen_clean.csv")   # O2 data



o2_extracted <- o2 %>%
  select("SAMPNO","OXYGEN_umolperkg")
o2_extracted$OXYGEN_FLAG_W <- NA

d_extracted <- merge(d,o2_extracted,by = "SAMPNO", all.x = TRUE)

head(o2_extracted)

s <- rbind(s,d_extracted)  
write.csv(s,"data/UNSUB_tracers_o2_2.csv")
