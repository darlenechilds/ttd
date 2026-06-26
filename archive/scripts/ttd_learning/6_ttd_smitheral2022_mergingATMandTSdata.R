rm(list = ls())
setwd("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/EN4/")

# path <- "E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/EN4/"
# files <- list.files(path = path, pattern = '*\\.csv', full.names = TRUE)
# s <- NULL
# for (i in 1:length(files)){
#   d <- read.csv(files[i], header = T, stringsAsFactors = F)
#   s <- rbind(s,d)  }
# write.csv(s,"EN4_TS_surface_labsea.csv")


sf6 <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/ttd_learning/atmos_data_sf6_comb.csv")
ts <- read.csv("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/EN4/EN4_TS_surface_labsea.csv")
  
nearest_match <- function(x, y, threshold = 0.1) {
  sapply(x, function(xi) {
    diffs <- abs(y - xi)
    if (min(diffs, na.rm = TRUE) <= threshold) {
      y[which.min(diffs)]
    } else {
      NA  # No match within threshold
    }
  })
}

# Match closest temp/sal year for each sf6 year
sf6$match_year <- nearest_match(sf6$Year, ts$Year)

# Merge using the matched year
merged <- merge(sf6, ts, by.x = "match_year", by.y = "Year", all.x = TRUE)
# merged_clean <- na.omit(merged)  # removes ~ 50 lines

# Interpolate missing data
library(zoo)

#  approx - draws a strait line between known points, na.approx is the timeseries friendly version
merged$MeanSal_filled <- na.approx(merged$MeanSal, x = merged$Year, rule = 2)
merged$MeanTemp_filled <- na.approx(merged$MeanTemp, x = merged$Year, rule = 2)

#  didnt extrapolate begining 10 years ... 
# merged$MeanTemp_filled <- na.approx(merged$MeanTemp, x = merged$Year   , na.rm = FALSE)
# merged$MeanSal_filled  <- na.approx(merged$MeanSal,  x = merged$Year   , na.rm = FALSE)

merged$temp_was_na <- is.na(merged$MeanTemp)
merged$sal_was_na  <- is.na(merged$MeanSal)

write.csv(merged,"atmos_data_sf6_comb_wTS.csv")

plot(merged$Year, merged$MeanTemp_filled, type = "l")
points(merged$Year, merged$MeanTemp, col = "red", pch = 16)



