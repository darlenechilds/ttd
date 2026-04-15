#read in data from OCADS website, extract data and save it in the data folder
# qa/qc ocads flags https://www.ncei.noaa.gov/products/ocean-carbon-acidification-data-system

rm(list = ls())
library(dplyr)
library(tidyverse)

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
  # "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108223/18HU20070510.exc.csv",
  
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108223/18HU20070510.exc.csv",
  
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108224/18HU20080520.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108073/18HU20090517.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108225/18HU20100513.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0108124/18HU20110506.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0144337/18MF20120601.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0144303/18HU20130507.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0157623/18HU20140502.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0160487/18HU20150504.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0186204/18HU20160430.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0237146/18HU20180425_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0237232/18DL20190601_hy1.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0240681/18DL20200722.exc.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0287917/33AT22805_data_v2.csv",
  "https://cchdo.ucsd.edu/data/42922/18QL23573_data.csv",  # no sf6 yet
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0313460/18QL20240527_data.csv",
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0313459/35A320250427_data.csv"
  
  )


# #check individual files
# checkfile <- read.csv(urls[31])

 d_check <- read_ocads(urls[20])
# lines <- readLines(urls[11])
# header_line <- grep("^EXPOCODE", lines)[1]


#figure out where the header is.
# lines <- readLines(urls[20], n = 50)
# cat(lines, sep = "\n")

#function to read in ocad data
read_ocads <- function(url) {
  tryCatch({
    
    # Read lines first
    lines <- readLines(url)
    
    # Find the header - EXPOCODE
    header_line <- grep("^EXPOCODE", lines)[1]
    
    # Read from that line
    df <- read.csv(text = lines[header_line:length(lines)],
                   stringsAsFactors = FALSE)
    
    df$source <- url
    
    return(df)
    
  }, error = function(e) {
    message(paste("Failed:", url))
    return(NULL)
  })
}



# d <- lapply(urls, read_ocads)
# d <- d[!sapply(d, is.null)]

save(d, file = './d.rda')
ocads_d <- './d.rda'
load(ocads_d)


#convert all to character to combine into one dataframe
d <- lapply(d, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})

#one dataframe with multiple headers ...
d <- bind_rows(d)
d <- d[-1, ]  # remove units row

d$year <- substr(d$DATE,1,4)
unique(d$year)

#clean column names
grep("CFC", names(d), value = TRUE)
grep("BTL", names(d), value = TRUE)

d <- d %>%
  mutate(
    cfc12 = coalesce(CFC.12, CFC12),
    cfc12_flag = coalesce(CFC.12_FLAG_W,CFC12_FLAG_W),
    LATITUDE = coalesce(BTL_LAT,LATITUDE),
    LONGITUDE = coalesce(BTL_LON,LONGITUDE),
    )

names(d)

#further cleaning
ind <- which(d$EXPOCODE == "END_DATA")
d <- d[-ind, ]

d_tracer <- d %>%
  select("EXPOCODE", "STNNBR", "BTLNBR", "SAMPNO","DATE",  "LATITUDE", "LONGITUDE",
         "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"OXYGEN" ,"OXYGEN_FLAG_W" , 
         "SF6","SF6_FLAG_W","cfc12","cfc12_flag","year")


#convert back to numeric
library(dplyr)
df <- df %>% 
  mutate(across(c(col1, col2), as.numeric))


d_tracer <- d_tracer %>%
  mutate(across(
    c(LATITUDE, LONGITUDE,
      CTDPRS, CTDTMP, CTDSAL, SALNTY,
      OXYGEN,
      SF6, cfc12),
    # ~ {
    #   x <- na_if(., "-999")
    #   x <- na_if(x, "-999.0")
    #   x <- na_if(x, "-999.000")
    as.numeric
    # }
  ))

#make sure we didnt loose anything. 
unique(d_tracer$year)

plot(d_tracer$LONGITUDE,d_tracer$LATITUDE)

#get ar7w line

# get ar7w line
# ar7w line 
la <- c(56.1147, 56.5450, 56.9568, 57.3775, 57.8003, 58.2158, 59.4832, 59.7440,
        59.9808, 59.0685, 58.7815)
lo <- c(-53.1142, -52.6807, -52.2390, -51.7847,-51.3437, -50.8832, -49.4660,
        -49.1693, -48.8963, -49.9507, -50.4468)

ar7w <- data.frame(
  lat = la,
  lon = lo
)

tol <- 0.2   # ~20 km-ish (roughly, good starting point)

ar7w_data <- bind_rows(lapply(1:nrow(ar7w), function(i) {
  
  d_tracer %>%
    filter(
      abs(LATITUDE - ar7w$lat[i]) < tol,
      abs(LONGITUDE - ar7w$lon[i]) < tol
    )
}))

plot(ar7w_data$LONGITUDE,ar7w_data$LATITUDE)

unique(ar7w_data$year)

write.csv(ar7w_data,"data/OCADS_tracers_o2_ar7w.csv")

