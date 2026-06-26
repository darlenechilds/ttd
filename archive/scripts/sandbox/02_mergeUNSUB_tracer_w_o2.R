# merge tracer data with oxygen data (UNSUB)
rm(list = ls())
library(dplyr)
library(oce)

#load data
d <- read.csv("data/UNSUB_tracers.csv")   #read in tracer data
d <- d[,-c(1,2)]
o2 <- read.csv("data/UNSUB_oxygen_clean.csv")   # O2 data



o2_extracted <- o2 %>%
  select("SAMPNO","OXYGEN_mlperl","OXYGEN_umolperkg")
o2_extracted$OXYGEN_FLAG_W <- NA

head(o2)
d_merge <- merge(d,o2_extracted,by = "SAMPNO", all.x = TRUE)

head(d_merge)

write.csv(d_merge,"data/UNSUB_tracers_o2.csv", row.names = FALSE)
