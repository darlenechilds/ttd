#CTD Plot

library(dplyr)
library(stringr)
library(oce)
library(fields)
library(RColorBrewer)
library(scico)
library(paletteer)
library(reshape2)
library(ggplot2)

rm(list = ls())

g = 9.81
rho0 = 1025

metadata <- read.csv("Metadata.csv")

metals <- metadata %>% filter(INSTRUMENT_TYPE == "CTD") %>% select(INSTRUMENT_TYPE,STATION,EVENT_ID)

metals <- metals %>% distinct(STATION,EVENT_ID, .keep_all = T)
metals <- metals[c(-1,-2,-48),]

#create CTD file name:
metals$EVENT_ID = str_pad(as.character(metals$EVENT_ID),side = "left",pad = "0",width = 3)

ctdpath = "R:/Science/BIODataSvc/ARC/Archive/ctd/2025/"
ctdfilename = paste0("CTD_LAT2025146_",metals$EVENT_ID,"_1_DN.ODF")

nbctd = length(ctdfilename)

statime = matrix("",nbctd)
storechl = matrix(NaN,401,nbctd)
storetemp = matrix(NaN,401,nbctd)
storesal = matrix(NaN,401,nbctd)
dcm <- rep(NaN,nbctd)

dfmld = NULL

vecint <- seq(0,200,0.5)

par(ask=F,mfcol = c(2,2))
for (i in 1:nbctd)
{
  print(c(i,ctdfilename[i]))
  tab = read.odf(paste0(ctdpath,ctdfilename[i]))
  ctd = as.ctd(tab)
  statime[i] = as.character(ctd@metadata$startTime)
  z =ctd[["pressure"]]
  chl = ctd[["fluorescence2"]]
  print(range(chl[1:300]))
  temperature = ctd[["temperature"]]
  salinity = ctd[["salinity2"]]
  timecast = tail(ctd[["time"]],n=1)
  sigmaT = ctd[["sigmaTheta"]]
  
  sstint <- approx(z,temperature,vecint,rule = 2)$y
  storetemp[,i] = sstint #chl[indz]

  salint <- approx(z,salinity,vecint,rule = 2)$y
  storesal[,i] = salint #chl[indz]

  chlint <- approx(z,chl,vecint,rule = 2)$y
  storechl[,i] = chlint #chl[indz]
  
  ind10 = which.min(abs(z-10))
  ind5 = which.min(abs(z-5))
  ind150 = which.min(abs(z-150))
  Delta_sigma = sigmaT[ind10] - sigmaT 
  
  indMLD1 = which.min(abs(Delta_sigma + 0.01))
  MLD1 = z[indMLD1]
  
  N2 = g/rho0 * (sigmaT[ind150] - sigmaT[ind5]) / abs(z[ind150] - z[ind5])
  
  chlint = approx(z,chl,1:200,rule = 2)$y
  chl.sm =  smooth.spline(1:200,chlint,df = 10)$y
  indchl = which.max(chl.sm)
  
  aux = data.frame(STATION = metals$STATION[i], MLD = MLD1, N2 = N2, DCM = indchl)
  dfmld = rbind(dfmld,aux)
  
  plot(sigmaT[1:200],-z[1:200],main = metals$STATION[i])
  points(sigmaT[c(indMLD1)],c(-MLD1),col=c(2,4),pch=16,cex=2)
  plot(temperature[1:200],-z[1:200],ylim = c(-200,0))
  abline(h=c(-MLD1))
  plot(salinity[1:200],-z[1:200],ylim = c(-200,0))
  abline(h=c(-MLD1))
  plot(chlint,-1*1:200,ylim = c(-200,0),lwd=2)
  lines(chl.sm,-1*1:200,col = "blue")
  points(chlint[indchl],-1* indchl,pch=16, col = "red",cex = 2)
  dcm[i] = indchl
}

SiFon = 20
# Plot temperature per stations
temperature = as.data.frame(storetemp)
names(temperature) = metals$STATION
temperature$Depth = vecint
temperature <- temperature %>% relocate(Depth)

tmp <- melt(temperature, id.vars = "Depth")
tmp <- tmp %>% rename(
  STATION = variable,
  Temperature = value
)
tmp$STATION <- factor(tmp$STATION, levels = unique(tmp$STATION))
ggplot(tmp,aes(x = STATION, y = Depth, fill = Temperature) ) + geom_tile() +
  scale_y_reverse() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_scico(palette = "vik",midpoint = 0, name = "")  + 
  theme(text = element_text(size = SiFon), legend.key.height = unit(3,"lines")) 
ggsave("Transect_Temperature.png",width = 16, height = 8,dpi = 600)

## Plot Salinity per stations
salinity = as.data.frame(storesal)
names(salinity) = metals$STATION
salinity$Depth = vecint
salinity <- salinity %>% relocate(Depth)

tmp <- melt(salinity, id.vars = "Depth")
tmp <- tmp %>% rename(
  STATION = variable,
  salinity = value
)
tmp$STATION <- factor(tmp$STATION, levels = unique(tmp$STATION))
ggplot(tmp,aes(x = STATION, y = Depth, fill = salinity) ) + geom_tile() +
  scale_y_reverse() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  paletteer::scale_fill_paletteer_c("viridis::plasma", name = "") + 
  theme(text = element_text(size = SiFon), legend.key.height = unit(3,"lines"))
ggsave("Transect_Salinity.png",width = 16, height = 8,dpi = 600)

# Plot Chl-a per station
chla = as.data.frame(storechl)
names(chla) = metals$STATION
chla$Depth = vecint
chla <- chla %>% relocate(Depth)

tmp <- melt(chla, id.vars = "Depth")
tmp <- tmp %>% rename(
  STATION = variable,
  chla = value
)
tmp$STATION <- factor(tmp$STATION, levels = unique(tmp$STATION))
ggplot() + geom_tile(data = tmp,aes(x = STATION, y = Depth, fill = chla)) + 
  scale_y_reverse() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_c(trans = "log10",name="",labels = c(0.1,1,10),breaks = c(0.1,1,10)) + 
  theme(text = element_text(size = SiFon), legend.key.height = unit(3,"lines")) +
#  geom_point(data = dfmld, aes(x = STATION, y = DCM),col= "white",size = 4,shape = 13,size=4,stroke = 2)
  geom_point(data = dfmld, aes(x = STATION, y = MLD),col= "white",size = 4) +
  geom_point(data = dfmld, aes(x = STATION, y =  - 180 - log10(N2)*60 ),col="red",size = 4)
  ggsave("Transect_Chla_MLD.png",width = 16, height = 8,dpi = 600)
#ggsave("Transect_Chla_DCM.png",width = 16, height = 8,dpi = 600)


