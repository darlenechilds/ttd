#read in data from OCADS website, extract data and save it in the data folder
# qa/qc ocads flags https://www.ncei.noaa.gov/products/ocean-carbon-acidification-data-system

rm(list = ls())
library(dplyr)

s <- NULL  # create blank data frame 

# Vector of URLs from the OCADS site
urls <- c(
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0113550/18HU19920527.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0113552/18HU19930617.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0113554/18HU19940524.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0115006/a01w_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0115010/18HU19960512.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0113557/18HU19970509.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0113610/ar07w_j_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108215/18HU19990627.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108216/18HU20000520.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108217/18HU20010530.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108218/18HU20020623.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108219/18HU20030713.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108220/18HU20040515.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108221/18HU20050526.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108222/18HU20060524.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108223/18HU20070510.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108224/18HU20080520.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108073/18HU20090517.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108225/18HU20100513.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108124/18HU20110506.exc.csv",
  
  
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0160487/18HU20150504.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0186204/18HU20160430.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0237146/18HU20180425_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0237232/18DL20190601_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0240681/18DL20200722.exc.csv",
  
  "https://www.ncei.noaa.gov/data/oceans/archive/arc0237/0302739/1.1/data/0-data/18QL23573_data.csv"
  )


fn <- urls[6]  # file working on - sadly, still manual b/c cant work around cfc header mismatch

# find which line the EXPOCODE starts adn extract data
lines <- readLines(fn)
header <- grep("^EXPO", lines, value = TRUE)
header <- strsplit(header, ",")[[1]]
skip_no <- grep("^EXPO", lines, value = F)

#read in ocads
d <- read.csv(fn, skip = 0, header = T)
units <- d[1,]  #double check units
d <- d[-1,]
colnames(d) <- header
d <- d[which(d$SF6_FLAG_W==2),]  # find sf6 and aux data using SF6_FLAG_W, 2 = good
plot(d$LONGITUDE,d$LATITUDE)


d_extracted <- d %>%
  select("EXPOCODE", "STNNBR", "DATE",  "LATITUDE", "LONGITUDE",
                              "SAMPNO","CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
                             "OXYGEN" ,"OXYGEN_FLAG_W" , "SF6","SF6_FLAG_W",
         "CFC-12","CFC-12_FLAG_W")

# notes - 2019,2018, 2016 cfc header "CFC-12"


colnames(d_extracted) <- c("EXPOCODE", "STNNBR", "DATE",  "LATITUDE",
                             "LONGITUDE","SAMPNO","CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
                             "OXYGEN" ,"OXYGEN_FLAG_W" , "SF6","SF6_FLAG_W",
                             "CFC12","CFC12_FLAG_W")



s <- rbind(s,d_extracted)


write.csv(s,"C:/Users/childsd/repospace/ttd/data/ocads2015_2020.csv",row.names = F)

