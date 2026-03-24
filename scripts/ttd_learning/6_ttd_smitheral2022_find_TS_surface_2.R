# get surface temp and sal in the l.sea for the past 50 years...

rm(list = ls())

setwd("E:/A_docs/2025/2025_labSea/reports/ventialation_labSea_2015_2025/data/data_raw_shortcuts")
dir()
fn <- dir()

d <- read.csv(fn[6],header =  T, stringsAsFactors = F)   #read in data
#peat at the data
str(d)
head(d)
names(d)

d_surface <- d[which(d$latitude>56 & d$latitude<59.1),]
d_surface <- d_surface[which(d_surface$PrDM <100),]
d_surface <- d_surface[which(d_surface$T090C>-100),]
d_surface <- d_surface[which(d_surface$Sal00>-100),]

# Get ar7w line only, ie. bio cruises
d_surface$expo_id <- as.numeric(substr(d_surface$G2expocode,1,2))
d_surface_ar7w <- d_surface[which(d_surface$expo_id==18),] 

# unique(d_surface_ar7w$G2expocode)#check cruises
year <- as.numeric(substr(d_surface$date,9,12))
yearly_means_df <- aggregate(cbind(d_surface$T090C, d_surface$Sal00) ~ year, data = d_surface, mean, na.rm = TRUE)

write.csv(yearly_means_df,"yearly_means_surfaceTS.csv")
