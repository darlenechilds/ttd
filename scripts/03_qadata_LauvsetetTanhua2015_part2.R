#now have means of all stations from the neadw water mass 220 km from ar7w line
rm(list = ls())
library(dplyr)
library(geosphere)
library(oce)

#ref data
rd <- read.csv("data/ref_means.csv")
rd <- rd[!duplicated(rd$mean_sf6),]

# get test cruise
d <- read.csv("data/OCADS_tracers_o2.csv")
ucruise <- unique(d$EXPOCODE)
d <- d[d$EXPOCODE==ucruise[3],]
plot(d$LONGITUDE,d$LATITUDE)
d <- d[d$LATITUDE>50,]
d$theta <- swTheta(d$CTDSAL,d$CTDTMP,d$CTDPRS)
d$sigma2 <- swSigma2(d$CTDSAL,d$theta, d$CTDPRS)

#find NEADW / lat part only to keep full profiles for interpolation
d_neadw <- d[d$LATITUDE > 56 & d$LATITUDE <60,]
# d_neadw <- d_neadw[d_neadw$sigma2 > 36.965 & d_neadw$sigma2 < 37.04,]


#create unique stations
d_neadw$year <- substr(d_neadw$DATE,1,4)
d_neadw$stn <- as.numeric(paste(d_neadw$year,d_neadw$STNNBR,sep = ""))

# Define a common pressure grid
zout <- seq(1700, 2800, by = 50)

# Split the data by event
profiles <- split(d_neadw, d_neadw$stn)
# Interpolate each sf6 profile to the pressure grid, zout, rule 2 = if outside range use endpoint
interp_sf6 <- lapply(seq_along(profiles), function(i) {
  p <- profiles[[i]]
  data.frame(profile = i,pressure = zout,
             sf6 = approx(p$CTDPRS, p$SF6,  xout = zout, rule = 2)$y,
             sigma2 = approx(p$CTDPRS , p$sigma2, xout = zout, rule = 2)$y
  )
})


#convert to dataframe (from list)
interp_sf6_df <- as.data.frame(do.call(rbind, interp_sf6))
head(interp_sf6_df)

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

# Put it all back together.  

#get station ids merged with means and sds
stn_id <- unique(d_neadw$stn)
profile <- 1:length(stn_id)
#make a new dataframe
df <- data.frame(profile = profile,stn = stn_id)
df2 <- merge(s,df,by = "profile")
d_means <- merge(df2,d_neadw,by = "stn",all.x = TRUE)

# test dataset!
d_means_u <- d_means[!duplicated(d_means$mean_sf6),]

write.csv(d_means_u,"data/d_means.csv")

