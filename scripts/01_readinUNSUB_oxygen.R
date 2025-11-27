#Get O2 data from SRC network, create dataframe to include O2 data with local tracer data

rm(list = ls())
s <- NULL
library(dplyr)
library(oce)
library(readxl)

year_datafiles <- data.frame(
  year = c(2015, 2016, 2018, 2019, 2020, 2022, 2023, 2024, 2025),
  fn = c("R:/Science/BIODataSvc/SRC/2010s/2015/HUD2015006/BioChem/Oxygen/HUD2015006_Oxygen_Rpt_Nov16_18.csv",
    "R:/Science/BIODataSvc/SRC/2010s/2016/HUD2016006/BioChem/DO_data/HUD2016006_Oxygen_Rpt_Nov16_18.csv",
    "R:/Science/BIODataSvc/SRC/2010s/2018/HUD2018008/BioChem/HUD2018008_Oxygen_JB.xls.xlsx",
    "R:/Science/BIODataSvc/SRC/2010s/2019/AMU2019001/BIOCHEM/O2data_AMU_2019001qc_final_JB.xlsx",
    "R:/Science/BIODataSvc/SRC/2020s/2020/AMU2020001/BioChem/O2data_AMU_2020001qc_JB.xlsx",
    "R:/Science/BIODataSvc/SRC/2020s/2022/AT4805/BIOCHEM/OXY/O2data_ATL_2022048qc.xls",
    "R:/Science/BIODataSvc/SRC/2020s/2023/CAR2023573/BIOCHEM/Oxygens/CAR2023573_Oxygens_JB.xlsx",
    "R:/Science/BIODataSvc/SRC/2020s/2024/CAR2024924/BioChem/Oxygens/CAR2024924_Oxygens_JB.xlsx",
    "R:/Science/BIODataSvc/SRC/2020s/2025/LAT2025146/BioChem/LAT2025146_ AZOMP_Winkler_QAQCd.xlsx"),  stringsAsFactors = FALSE)


year <- 2015  # sadly this is a manual process 
row <- year_datafiles[year_datafiles$year == year, ]
fn <- row$fn

# Load data, 2015&2016 = csv, 2018+ = .xls
d <- read.csv(fn,header =  T, stringsAsFactors = F)   #read in data
head(d)

d <- read_excel(fn, sheet = 1, skip = 0)
head(d)

# Select columns by name
#CSV
d_extracted <- d %>%
  select("Event","SAMPLE_ID","START_DEPTH","Oxy_CTD_P","Oxy_CTD_S","Oxy_W_Rep1")
colnames(d_extracted) <- c("STNNBR","SAMPNO", "CTDPRS","CTDTMP", "CTDSAL","OXYGEN_mlperl")
head(d_extracted)
#2018-2022
# d_extracted <- d %>%
#   select("Event number","Sample ID","pressure","temp","salinity","O2 Winkler")
# colnames(d_extracted) <- c("STNNBR","SAMPNO", "CTDPRS","CTDTMP", "CTDSAL","OXYGEN_mlperl")
# head(d_extracted)

#2023,2024, 2025 no ctd data, get t, s, p, get qat file...
# qat <- read.csv("E:/A_docs/2023/2023_LabSea/QAT.csv") #2023
# qat <- read.csv("R:/Science/BIODataSvc/SRC/2020s/2024/CAR2024924/CTD/CTD_PROCESSING/2024924CAR/QAT/CAR2024924_QAT.csv") #2024
# qat <- read.csv("R:/Science/BIODataSvc/SRC/2020s/2025/LAT2025146/CTD/ORIGINAL/CTD_PROCESSING/2025146LAT/QAT/LAT2025146_QAT.csv")
head(qat)
qat_ex <- qat %>%
  select("sample_id","PrDM","T090C","Sal00")
colnames(qat_ex) <- c("SAMPNO", "CTDPRS","CTDTMP", "CTDSAL")

d$SAMPNO <- as.numeric(substr(d$Sample,1,6))

d_extracted <- d %>%
  select("sample_id","O2_Concentration(ml/l)")
colnames(d_extracted) <- c("SAMPNO","OXYGEN_mlperl")
head(d_extracted)

d_extracted <- merge(d_extracted,qat_ex,by = "SAMPNO", all.x = T)
d_extracted$STNNBR <- NA

d_extracted <- d_extracted %>%
  select("STNNBR","SAMPNO","CTDPRS","CTDTMP","CTDSAL","OXYGEN_mlperl")
head(d_extracted)
d_extracted$OXYGEN_mlperl <- as.numeric(d_extracted$OXYGEN_mlperl)


#convert DO ml/l to umol/kg - (1 kg/m^3 = 0.001 kg/L)
theta <- swTheta(d_extracted$CTDSAL, d_extracted$CTDTMP, d_extracted$CTDPRS, referencePressure = 0)
dens <- (swRho(d_extracted$CTDSAL,theta,d_extracted$CTDPRS))/1000  # kg/L
conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volumn, 1 mole of O2 = 22.4 L)
d_extracted$OXYGEN_umolperkg <- d_extracted$OXYGEN_mlperl*conv/dens

head(d_extracted)
s <- rbind(s,d_extracted)  
write.csv(s,"UNSUB_oxygen.csv")

# fix oxygen file using correct density
d <- read.csv("data/UNSUB_oxygen_3.csv")
head(d)

theta <- swTheta(d$CTDSAL, d$CTDTMP, d$CTDPRS, referencePressure = 0)
dens <- (swRho(d$CTDSAL,theta,d$CTDPRS))/1000  # kg/L
conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volumn, 1 mole of O2 = 22.4 L)
d$OXYGEN_umolperkg <- d$OXYGEN_mlperl*conv/dens

write.csv(d,"data/UNSUB_oxygen.csv")


