#  Inspired by Zeliang Wang; Can oxygen measurements be used to calculated the MLD?

rm(list = ls())
library(dplyr)
library(oce)

#load data - UNSUB - will leave for now.
# d <- read.csv("data/UNSUB_tracers_o2.csv")   #read in tracer data
# o2_flag <- which(d$OXYGEN_umolperkg<220)
# d <- d[-o2_flag,]
# o2 <- d[!is.na(d$OXYGEN_umolperkg), ]  # remove o2 nans

#load data - OCADS as i dont want to question the data at this point
d <- read.csv("data/OCADS_tracers_o2.csv")   #read in tracer data
# extract good data, flag = 2

d <- d[which(d$OXYGEN_FLAG_W==2),]


# calculated theoretical DO using Weiss 1970 solubility constants
T_k <- d$CTDTMP+273.15
s <- d$CTDSAL
Ln_F <- -173.4965+249.6339*(100/T_k)+143.3483*log(T_k/100)+-21.8492*(T_k/100)+
            s*(-0.033096+0.014259*(T_k/100)+-0.0017000*(T_k/100)^2)

d$eF_ml_per_L <- exp(Ln_F) # ml_per_L

#convert DO ml/l to umol/kg - (1 kg/m^3 = 0.001 kg/L)
theta <- swTheta(d$CTDSAL, d$CTDTMP, d$CTDPRS, referencePressure = 0)
dens <- (swRho(d$CTDSAL,theta,d$CTDPRS))/1000  # kg/L
conv <-  44.66 #(1 ml O2 = 44.66 umol O2 @ stp, derived from gas law, molar volumn, 1 mole of O2 = 22.4 L)
d$eF_umol_per_kg <- d$eF_ml_per_L*conv/dens

head(d)

d$do_sat <- d$OXYGEN/d$eF_umol_per_kg*100
d$aou <- d$eF_umol_per_kg-d$OXYGEN

#define stations 14-19 as in Lazier et al., 2002
lswc <- d[which(d$LATITUDE > 56.5 & d$LATITUDE <59),]  
lswc <- lswc[which(lswc$CTDPRS > 200),]  
lswc <- lswc[which(lswc$LONGITUDE > -53),]   

#ensure no offline (off ar7w) data
plot(lswc$LONGITUDE,lswc$LATITUDE)
plot(lswc$do_sat,-lswc$CTDPRS)

# quick qa/qc check

ucruise <- unique(lswc$EXPOCODE)
i <- ucruise[2]
j <- ustn[4]
for (i in ucruise){
  e <- lswc[which(lswc$EXPOCODE==i),]
  ustn <- as.numeric(unique(e$STNNBR))
  for (j in ustn){
    f <- e[which(e$STNNBR==j),]
    par(mfrow = c(1,2), mar = c(5, 2, 3, 1 ))
    plot(f$OXYGEN,-f$CTDPRS,pch = 16, xlim = c(250, 350), ylim = c(-3700,0), main = paste(i,j))
    points(f$eF_umol_per_kg,-f$CTDPRS, pch = 16, col = "blue")
    plot(f$do_sat,-f$CTDPRS,pch = 16, xlim = c(90,105),ylim = c(-3700,0))
    # plot(f$CTDTMP, -f$CTDPRS,pch = 16, xlim = c(-1.5,5),ylim = c(-3700,0))
    # plot(f$CTDSAL, -f$CTDPRS,pch = 16, xlim = c(34.7, 35),ylim = c(-3700,0))
  }
}

flags <- c(437757, 437659, 476671, 476619)
ind <- which(lswc$SAMPNO %in% flags)
lswc <- lswc[-ind,]

#plot profiles for each cruise
for (i in ucruise){
  e <- lswc[which(lswc$EXPOCODE==i),]
  plot(e$OXYGEN,-e$CTDPRS,pch = 16, xlim = c(250, 350), ylim = c(-3700,0), main = paste(i))
}



#---find MLD for each profile for each cruise.  2020 - hard due to mininal samples taken 

th <- 98

mld <- lswc |>
  group_by(EXPOCODE, STNNBR) |>
  summarise(
    max_depth = max(CTDPRS[do_sat >= threshold], na.rm = TRUE),
    .groups = "drop"
  )


# concluding calculating the mld from oxygen data is  too sparce
# and too variable to obtain an consistent number.  

