#read in data from OCADS website, extract data and save it in the data folder
# qa/qc ocads flags https://www.ncei.noaa.gov/products/ocean-carbon-acidification-data-system

# Step 1: read all OCADS files
# Step 2: standardize column names
# Step 3: combine datasets


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
  "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0228686/33AT20220322_data.csv",
  "https://www.ncei.noaa.gov/data/oceans/archive/arc0237/0302739/1.1/data/0-data/18QL23573_data.csv"
  )

#figure out where the header is.
lines <- readLines(urls[20], n = 50)
cat(lines, sep = "\n")

#function to read in ocad data
read_ocads <- function(url) {
  tryCatch({
    
    # Read all lines first
    lines <- readLines(url)
    
    # Find the real header (starts with EXPOCODE)
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

d <- lapply(urls, read_ocads)
d <- d[-1, ]  # remove units row

#convert all to character to combine into one dataframe
d <- lapply(d, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})

#one dataframe with multiple headers ...
d <- bind_rows(d)
names(d)

#clean column names
grep("CFC", names(d), value = TRUE)

d <- d %>%
  mutate(
    cfc12 = coalesce(CFC.12, CFC12),
    cfc12_flag = coalesce(CFC.12_FLAG_W,CFC12_FLAG_W),
    )

#convert back to numeric
d <- d %>%
  mutate(across(everything(), ~ as.numeric(.)))

#get tracer data base with oxygen

d_tracer <- d %>%
  select("EXPOCODE", "STNNBR", "BTLNBR", "DATE",  "LATITUDE", "LONGITUDE",
         "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,"OXYGEN" ,"OXYGEN_FLAG_W" , 
         "SF6","SF6_FLAG_W","cfc12","cfc12_flag")

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

write.csv(ar7w_data,"data/OCADS_tracers_o2.csv")

