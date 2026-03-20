#uses crossover method from  Lauvset$Tanhua2015 to det. accuracy bias
#data from GLODAP to be used as reference cruises for qa process
#im sad i could not get the url to work from the https://glodap.info/index.php/merged-and-adjusted-data-product-v2-2023/ site
#i just downloaded the zip file and put it in the data folder :(

rm(list = ls())
library(dplyr)
library(geosphere)
library(oce)

# fn <- c("https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0283442/GLODAPv2.2023_Atlantic_Ocean.csv")
fn <- "data/GLODAPv2.2023_Atlantic_Ocean.csv"

#read in ref data
ref_d <- read.csv(fn,  header = T)
ref_d <- ref_d[ref_d$G2sf6f==2,]  # get good sf6 data
ref_d <-  ref_d[!is.na(ref_d$G2expocode), ]

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
dis <- dist2Line(p = ref_d_lsw[, c("G2longitude", "G2latitude")],line = transect)  # meters
ref_d_lsw$dis <- dis[, "distance"]

# Keep those within 2° arc distance
ref_2deg <- ref_d_lsw[ref_d_lsw$dis <= max_m, ]

points(ref_2deg$G2longitude,ref_2deg$G2latitude)

#find NEADW / lat part only to keep full profiles for interpolation
ref_2deg$sigma2 <- swSigma2(ref_2deg$G2salinity,ref_2deg$G2theta, ref_2deg$G2pressure)
ref_neadw <- ref_2deg[ref_2deg$G2latitude > 56 & ref_2deg$G2latitude <60,]
# ref_neadw <- ref_neadw[ref_neadw$sigma2 > 36.965 & ref_neadw$sigma2 < 37.04,]
points(ref_neadw$G2longitude, ref_neadw$G2latitude, col = "red")

#interpolate profiles
#make unique stations 
ref_neadw$stn <- as.numeric(paste(ref_neadw$G2year,ref_neadw$G2station,sep = ""))

# Define pressure grid
zout <- seq(1500, 2800, by = 50)

# Split the data by event
profiles <- split(ref_neadw, ref_neadw$stn)
names(profiles)

# Interpolate each sf6 profile to the pressure grid, zout, rule 2 = if outside range use endpoint
interp_sf6 <- lapply(seq_along(profiles), function(i) {
  p <- profiles[[i]]
    data.frame(profile = i,pressure = zout,
    sf6 = approx(p$G2pressure, p$G2sf6,  xout = zout, rule = 2)$y
  )
})

#give header names
interp_sf6 <- lapply(interp_sf6, function(x) {
  names(x) <- c("profile", "pressure","sf6")
  return(x)
})

#convert to dataframe (from list)
interp_sf6_df <- as.data.frame(do.call(rbind, interp_sf6))
head(interp_sf6_df)


#merge with meta data
stn_id <- unique(ref_neadw$stn)
profile <- 1:138
#make a new dataframe
df <- data.frame(profile = profile,stn = stn_id)
df2 <- merge(interp_sf6_df,df,by = "profile")

#make dataframe of meta data to merge with interp profiles
meta <- ref_neadw %>%
  select("G2expocode", "G2station", "G2year",  "G2latitude", "G2longitude",
         "dis", "stn")
unique(meta$G2station)

#interp_w_meta <- merge(df2,meta,by = "stn")

# ref_interp <- merge(df2,ref_neadw,by = "stn",all.x = TRUE)






#calculate the mean of each profile for the neadw
#find neadw, part 2, i.e. sigma2
ref_neadw_2 <- interp_sf6_df[interp_sf6_df$sigma2 > 36.965 & interp_sf6_df$sigma2 < 37.04,]
ustn <- unique(ref_neadw_2$profile)
i <- ustn[3]
s <- NULL
for (i in ustn){
  e <- ref_neadw_2[ref_neadw_2$profile==i,]
  e <- e[!is.na(e$sf6),]
  meansf6_neadw <- mean(e$sf6)
  sdsf6_neadw <- sd(e$sf6)
  f <- data.frame(profile = i,mean_sf6 = meansf6_neadw,sd_sf6 = sdsf6_neadw)
  s <- rbind.data.frame(s,f)
}
head(s)
# need to put it all back together.  

#need to get station ids merged with means and sds
stn_id <- unique(ref_neadw$stn)
profile <- 1:138
#make a new dataframe
df <- data.frame(profile = profile,stn = stn_id)
df2 <- merge(s,df,by = "profile")
ref_means <- merge(df2,ref_neadw,by = "stn",all.x = TRUE)

#us this to compare test dataset to!
ref_means_u <- ref_means[!duplicated(ref_means$mean_sf6),]

write.csv(ref_means_u,"data/ref_means.csv")

