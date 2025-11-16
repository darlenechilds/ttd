#read in data from OCADS website, extract data and save it in the data folder
rm(list = ls())
library(dplyr)

s <- NULL  # create blank data frame 

# Vector of URLs from the OCADS site
urls <- c(
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0240681/18DL20200722.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0237232/18DL20190601_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0237146/18HU20180425_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0186204/18HU20160430.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0160487/18HU20150504.exc.csv"
)


fn <- urls[5]

# find which line the EXPOCODE starts adn extract data
lines <- readLines(fn)
header <- grep("^EXPO", lines, value = TRUE)
header <- strsplit(header, ",")[[1]]
skip_no <- grep("^EXPO", lines, value = F)

#read in ocads
d <- read.csv(fn, skip = skip_no, header = F)
units <- d[1,]  #double check units
d <- d[-1,]
colnames(d) <- header
d <- d[which(d$SF6_FLAG_W==2),]  # find sf6 and aux data using SF6_FLAG_W, 2 = gooda
plot(d$LONGITUDE,d$LATITUDE)


d_extracted <- d %>%
  select("EXPOCODE", "STNNBR", "DATE",  "LATITUDE",
                             "LONGITUDE", "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
                             "OXYGEN" ,"OXYGEN_FLAG_W" , "SF6","SF6_FLAG_W",
         "CFC-12","CFC-12_FLAG_W")

# notes - 2019,2018, 2016 cfc header "CFC-12"


colnames(d_extracted) <- c("EXPOCODE", "STNNBR", "DATE",  "LATITUDE",
                             "LONGITUDE", "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
                             "OXYGEN" ,"OXYGEN_FLAG_W" , "SF6","SF6_FLAG_W",
                             "CFC12","CFC12_FLAG_W")



s <- rbind(s,d_extracted)


"../data/another_file.txt"


write.csv(s,"C:/Users/childsd/repospace/ttd/data/ocads2015_2020.csv",row.names = F)
