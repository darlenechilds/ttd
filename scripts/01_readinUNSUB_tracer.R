#compiles unsubmitted data from "all_chem" type files which contains data submitted to teh data shop, files are not consitent wrt headers, structure, etc., its a manual process

rm(list = ls())
# s <- NULL
library(dplyr)
library(oce)

year_info <- data.frame(
  year = c(2015, 2016, 2018, 2019, 2020, 2022, 2023, 2024, 2025),
  sf6_p_atm = c(0.00000000000874, 0.00000000000911, 0.00000000000975, 0.00000000001012, 0.00000000001045, 0.00000000001122, 0.00000000001166, 0.00000000001254, 0.00000000001272),
  fn = c(
    "E:/A_docs/2010s/2015/2015_HUD006/fr_tracer/HUD2015006_all_Chem_ar7w.csv",
    "E:/A_docs/2010s/2016/2016_006_labsea/fr_tracer/HUD2016006 all chem data Final_ar7w.csv",
    "E:/A_docs/2010s/2018/2018_HUD2018008/HUD2018-008 all chem data flagged 1st Nov 2018_ar7w_sf6f12.csv",
    "E:/A_docs/2010s/2019/2019_LabSea/AMU2019-001 all chem data flagged 12 Nov 19_ar7w_sf6f12.csv",
    "E:/A_docs/2020/2020_labsea/2020_Amundsen_labsea/AMU2020-001 Tracers with gravimetric data_fix_ar7w.csv",
    "E:/A_docs/2022/2022_labsea_AZOMP/DATA/2022_LabSea_allchem_AR7W_sf6.csv",
    "E:/A_docs/2023/2023_LabSea/2023_LabSea_allchem_AR7W_sf6.csv",
    "E:/A_docs/2024/2024_labsea/2024_tracerdata_saturations_ar7w.csv",
    "E:/A_docs/2025/2025_labSea/2025_labsea/raw_seawater/2025_ar7w_tracers.csv"
  ),  stringsAsFactors = FALSE)


year <- 2016  # sadly this is a manual process as variables/headers/etc are inconsistent
row <- year_info[year_info$year == year, ]
fn <- row$fn

# Load data
d <- read.csv(fn,header =  T, stringsAsFactors = F)   #read in data
head(d)
d$SALNTY <- NA
d$SF6_FLAG_W <- NA
d$CFC12_FLAG_W <- NA

# Select columns by name
d_extracted <- d %>%
  select("cruise_number", "event", "Date","sample_id",
         "latitude","longitude", "PrDM","T090C", "Sal00", "SALNTY",
         "sf6_fmolperkg","SF6_FLAG_W","f12_pmolperkg","CFC12_FLAG_W")

colnames(d_extracted) <- c("EXPOCODE", "STNNBR", "DATE", "SAMPNO", 
                           "LATITUDE","LONGITUDE", "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
                            "SF6","SF6_FLAG_W","CFC12","CFC12_FLAG_W")

head(d_extracted)

#------------------
# 2025, duplicate measurements were taken at every station
dups <- d_extracted[duplicated(d_extracted$SAMPNO) | duplicated(d_extracted$SAMPNO, fromLast = TRUE), ]
ind <- which(d_extracted$SAMPNO %in% dups$SAMPNO)
d_extracted <- d_extracted[-ind,]  # remove duplicats, will get them back in later
# compute mean + sd for SF6 and f12per SAMPNO
stats <- aggregate(cbind(SF6 , CFC12) ~ SAMPNO, data = dups,
                   FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                       sd   = sd(x, na.rm = TRUE)))

stats$CFC12_mean <- stats$CFC12[, "mean"]
stats$CFC12_sd   <- stats$CFC12[, "sd"]
stats$OXYGEN <- NULL

stats$SF6_mean <- stats$SF6[, "mean"]
stats$SF6_sd   <- stats$SF6[, "sd"]
stats$SF6 <- NULL

dups_final <- merge(dups, stats, by = "SAMPNO", all.x = TRUE)

dups_extracted <- dups_finala %>%
  select("EXPOCODE", "STNNBR", "DATE", "SAMPNO", 
          "LATITUDE","LONGITUDE", "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
          "SF6_mean","SF6_FLAG_W","CFC12_mean","CFC12_FLAG_W")

colnames(dups_extracted) <- c("EXPOCODE", "STNNBR", "DATE", "SAMPNO", 
                           "LATITUDE","LONGITUDE", "CTDPRS","CTDTMP", "CTDSAL", "SALNTY" ,
                           "SF6","SF6_FLAG_W","CFC12","CFC12_FLAG_W")

d_extracted <- rbind.data.frame(d_extracted,dups_extracted)
#----------
#correct cruise id for 2018
# unique(d_extracted$EXPOCODE)
# d_extracted$EXPOCODE <- rep("HUD2018008",length(d_extracted$EXPOCODE))

#---------
#fix cruise id for 2022
s$EXPOCODE[s$EXPOCODE == "AT4802 AT4805"] <- "AT4805"
s$EXPOCODE[s$EXPOCODE == "AT4802"] <- "AT4805"
unique(s$EXPOCODE)



s <- rbind(s,d_extracted)  
write.csv(s,"data/UNSUB_tracers.csv")

#remove duplicates as 2016 was added to s 2x, one day i promise to do better...
s <- read.csv("data/UNSUB_tracers_o2a.csv")
dup <- s[duplicated(s$SAMPNO),]
s <- s[!duplicated(s$SAMPNO), ]
#check
sunique <- unique(s$SAMPNO)  
unique(s$EXPOCODE)

