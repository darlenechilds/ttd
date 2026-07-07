#thinking work to figure out the function clean_ocads_columns/read_ocads_file

d <- read.csv("data/raw/ocads_urls.csv")

url <- d$url
i <- url[1]

url <- url[5]  #to test read_ocads_file  function

#look at each header for each year
header_list <- NULL
for (i in url){
  lines <- readLines(i)
  header_line <- grep("^EXPOCODE", lines)[1]
  header <- lines[header_line]
  header_list <- rbind(header_list, header)}
# header_list <- strsplit(header_list, ",")
# header_list <- cbind(d$year,header_list)
header_list[[1]]  

#look at missing data/clean names
i <- url[1]
e <- read_ocads(i)

e <- e[e$EXPOCODE != "END_DATA" &
         !is.na(e$CTDPRS) &
         e$CTDPRS != "DBARS",]



unique(e$EXPOCODE)




##  get wanted variables
vars <- c("EXPOCODE","STNNBR","CASTNO","SAMPNO","DATE","TIME",
          "LATITUDE","LONGITUDE","CTDPRS","CTDTMP","CTDSAL",
          "SALNTY","SALNTY_FLAG_W",
          "CFC12","CFC12_FLAG_W","SF6","SF6_FLAG_W",
          "CTDOXY","OXYGEN","OXYGEN_FLAG_W",
          "TCARBN","TCARBN_FLAG_W","ALKALI","ALKALI_FLAG_W")

names(e)

if ("CFC-12" %in% names(e)) {
  names(e)[names(e) == "CFC-12"] <- "CFC12"}
if ("CFC-12_FLAG_W" %in% names(e)) {
  names(e)[names(e) == "CFC-12_FLAG_W"] <- "CFC12_FLAG_W"}
if ("CFC.12" %in% names(e)) {
  names(e)[names(e) == "CFC.12"] <- "CFC12"}
if ("CFC.12_FLAG_W" %in% names(e)) {
  names(e)[names(e) == "CFC.12_FLAG_W"] <- "CFC12_FLAG_W"}


if ("BTL_LAT" %in% names(e)) {
  names(e)[names(e) == "BTL_LAT"] <- "LATITUDE"}
if ("BTL_LON" %in% names(e)) {
  names(e)[names(e) == "BTL_LON"] <- "LONGITUDE"}

missing <- setdiff(vars, names(e))
missing




#build column for missing data so all teh data can merge together

for(i in missing){
  e[[i]] <- NA
}
e <- e[vars]






# now want to exctend to other variables

#generic select variables function 
select_variables <- function(d, vars){
  
  missing <- setdiff(vars, names(d))
  
  for(i in missing)
    d[[i]] <- NA
  
  d[vars]
}


tracer_vars <- c(...)
oxygen_vars <- c(...)
nutrient_vars <- c(...)

tracers  <- select_variables(d, tracer_vars)

oxygen   <- select_variables(d, oxygen_vars)

nutrients <- select_variables(d, nutrient_vars)









