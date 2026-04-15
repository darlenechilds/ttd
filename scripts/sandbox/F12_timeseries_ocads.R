#timeseries plot for f12for different water masses using ocads data

rm(list = ls())

#compiled from 00_readinOCADS_tracer_o2 script
d <- read.csv("data/OCADS_tracers_o2.csv", stringsAsFactors = F)

#get F12 dataframe
d <- d%>%
  select("EXPOCODE", "STNNBR", "BTLNBR", "DATE",  "LATITUDE", "LONGITUDE",
         "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"cfc12","cfc12_flag")


#filter/keep good data usign ocads quality control flags, 2 = good, 6 = median of replicates
d <- d[d$cfc12_flag==2 | d$cfc12_flag==6,]

#check
plot(d$LONGITUDE,d$LATITUDE)
plot(d$cfc12,-d$CTDPRS)
#remove f12 = 0
d <- d[d$cfc12 != 0, ]
#remove na from expcode
d <- d[!is.na(d$EXPOCODE), ]


#add year
d$year <- as.numeric(substr(d$DATE,1,4))
head(d)

unique(d$year)
