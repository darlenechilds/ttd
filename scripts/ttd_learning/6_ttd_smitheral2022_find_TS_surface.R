# get surface temp and sal in the l.sea for the past 50 years...


rm(list = ls())
# s <- NULL

library(dplyr)

setwd("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/glodap_sf6")

dir()
fn <- dir()

d <- read.csv(fn[25],header =  T, stringsAsFactors = F)   #read in data
#peat at the data
str(d)
head(d)
names(d)


# get decimal year; 
d$Date <- as.Date(with(d, paste(d$G2year, d$G2month, d$G2day, sep = "-")))
year <- d$G2year
day_of_year <- as.numeric(format(d$Date, "%j"))
is_leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
days_in_year <- ifelse(is_leap, 366, 365)
d$decimal_year <- year + (day_of_year - 1) / days_in_year



#find surface

d_surface <- d[which(d$G2latitude>56 & d$G2latitude<59.1),]
d_surface <- d_surface[which(d_surface$G2pressure<100),]
d_surface <- d_surface[which(d_surface$G2temperature>-100),]
d_surface <- d_surface[which(d_surface$G2salinity>-100),]

# Get ar7w line only, ie. bio cruises
d_surface$expo_id <- as.numeric(substr(d_surface$G2expocode,1,2))
d_surface_ar7w <- d_surface[which(d_surface$expo_id==18),] 

# unique(d_surface_ar7w$G2expocode)#check cruises

yearly_means_df <- aggregate(cbind(d_surface_ar7w$G2temperature, d_surface_ar7w$G2salinity) ~ d_surface_ar7w$decimal_year, data = d_surface_ar7w, mean, na.rm = TRUE)


write.csv(yearly_means_df,"Glodap_v2_yearly_means_surfaceTS.csv")
dir()
