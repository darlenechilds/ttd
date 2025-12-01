#uses crossover method from  Lauvset$Tanhua2015 to det. accuracy bias
#data from GLODAP to be used as reference cruises for qa process
#im sad i could not get the url to work from the https://glodap.info/index.php/merged-and-adjusted-data-product-v2-2023/ site
#i just downloaded the zip file and put it in the data folder :(

rm(list = ls())
library(dplyr)
library(geosphere)



# fn <- c("https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0283442/GLODAPv2.2023_Atlantic_Ocean.csv")
fn <- "data/GLODAPv2.2023_Atlantic_Ocean.csv"

#read in ref data
ref_d <- read.csv(fn,  header = T)
ref_d <- ref_d[!is.na(ref_d$G2expocode), ]

#get variables
ref_d <- ref_d %>%
  select("G2expocode", "G2station", "G2year",  "G2latitude", "G2longitude",
         "G2pressure", "G2theta", "G2salinity",
         "G2oxygen" ,"G2oxygenf" , "G2sf6","G2sf6f",
         "G2cfc12","G2cfc12f")


#get lab sea data and/or ar7w line used script plot chart lsw to find area 
# Define bounds
lat_min <- 50.5
lat_max <- 62.3
lon_min <- -65.5
lon_max <- -32.2
ref_d_lsw <- ref_d[ref_d$G2latitude >= lat_min & ref_d$G2latitude <= lat_max &
    ref_d$G2longitude >= lon_min & ref_d$G2longitude <= lon_max,]
#check
range(ref_d_lsw$G2latitude)
range(ref_d_lsw$G2longitude)

# look at number of different cruises
unique(ref_d_lsw$G2expocode)
plot(ref_d_lsw$G2longitude,ref_d_lsw$G2latitude)

#find NEADW
ref_d_lsw$sigma2 <- swSigma2(ref_d_lsw$G2salinity,ref_d_lsw$G2theta, ref_d_lsw$G2pressure)
ref_neadw <- ref_d_lsw[ref_d_lsw$G2latitude > 56 & ref_d_lsw$G2latitude <60,]
ref_neadw <- ref_neadw[ref_neadw$sigma2 > 36.965 & ref_neadw$sigma2 < 37.04,]

points(ref_neadw$G2longitude, ref_neadw$G2latitude, col = "red")

#-- get cruise would like to test
d <- read.csv("data/OCADS_tracers_o2.csv")
ucruise <- unique(d$EXPOCODE)
d <- d[d$EXPOCODE==ucruise[3],]
plot(d$LONGITUDE,d$LATITUDE)
d <- d[d$LATITUDE>50,]
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta, d$CTDPRS)
d_neadw <- d[d$LATITUDE > 56 & d$LATITUDE <60,]
d_neadw <- d_neadw[d_neadw$sigma2 > 36.965 & d_neadw$sigma2 < 37.04,]

# get reference stations within ~200km (2 degrees arcdistance)
# ar7w line - neadw stations
la <- c(56.1147, 56.5450, 56.9568, 57.3775, 57.8003, 58.2158, 59.4832, 59.7440,
        59.9808, 59.0685, 58.7815)
lo <- c(-53.1142, -52.6807, -52.2390, -51.7847,-51.3437, -50.8832, -49.4660,
        -49.1693, -48.8963, -49.9507, -50.4468)

transect <- cbind(lo, la)  

# Convert 2 degrees to meters
max_m <- 2 * 111000  # ~222 km

# Compute distance from each station to the whole line
ref_neadw <-  ref_neadw[!is.na(ref_neadw$G2expocode), ]
dis <- dist2Line(p = ref_neadw[, c("G2longitude", "G2latitude")],line = transect)  # meters

ref_neadw$dist_to_line_m <- dis[, "distance"]

# Keep those within 2° arc distance
ref_neadw_2deg <- ref_neadw[ref_neadw$dist_to_line_m <= max_m, ]
# keep data with sf6 meansurements
ref_neadw_2deg <- ref_neadw_2deg[ref_neadw_2deg$G2sf6f==2,]
plot(ref_neadw_2deg$G2longitude,ref_neadw_2deg$G2latitude)
unique(ref_neadw_2deg$G2expocode)

## things are getting messy but we do have ref stations (stations_within_2deg) and test stations (d_neadw)
# need to interpolate profiles for both ref and test stations

unique(ref_neadw_2deg$G2station)



