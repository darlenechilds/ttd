# figure for meeting with Dalhousie. 

library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(lubridate)

library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggOceanMaps)

library(ggspatial)

# color from Kristen
# in.col = stepped(24)[c(5,7,9,11,13,15,17,19)]

rm(list=ls())

tab <- read.csv("C:/Users/devrede/Documents/GitHub/BloomStudy_LabSea/data/AddinCDOMJan142026/AZOMP_Taxonomy_2000-2025_cdom_20260114.csv")
#tab <- read.csv("C:/Users/devrede/Desktop/TMP/AZOMP/2025/Meeting_Dal_January_2025/AZOMP_Taxonomy_2000-2025_20260112.csv")
tab$Datetime <- paste0(tab$DATE,tab$TIME)
tab$DATE = as.Date(tab$DATE,format = "%m/%d/%Y")

nutrients <- read.csv("AZOMP_nutrients_2025.csv")

indm = nutrients$SAMPLE_ID %in% tab$SAMPLE_ID
indm2 = tab$SAMPLE_ID %in% nutrients$SAMPLE_ID

tab[indm2,14:18] = nutrients[indm,2:6]

tab25surf <- tab %>% filter(YEAR >= 2025 & DEPTH < 5)

#### Let try to find the missing nutrients as they come from a different bottle (the one just before)
misnut <- tab25surf %>% filter(is.na(NITRATE)) %>% mutate(SAMPLE_ID = SAMPLE_ID-1)

indm = misnut$SAMPLE_ID %in% nutrients$SAMPLE_ID
indm2 = nutrients$SAMPLE_ID %in% misnut$SAMPLE_ID

misnut[,14:18] = nutrients[indm2,2:6]
indna = is.na(tab25surf$NITRATE)

tab25surf[indna,14:18] = misnut[,14:18]

tab25surf <- tab25surf %>% mutate(
  pctdiatom = diatomC4/CHLA,
  pctdino = dino1C4/CHLA,
  pctphaeo = phaeoC4/CHLA,
  pctprasino = prasino2C4/CHLA,
  pctcrypto = cryptoC4/CHLA,
  pcthapto = hapto6C4/CHLA,
  pctdictyo = dictyoC4/CHLA,
  pctchloro = chloroC4/CHLA,
  pctother = otherPhyto/CHLA
  )

tab25surf$ZONE = "AR7W1"
tab25surf$ZONE[9:17] = "NROI"
tab25surf$ZONE[18:27] = "AR7W2"
tab25surf$ZONE[28:31] = "CROI"
tab25surf$ZONE[32:40] = "AR7W3"
tab25surf$ZONE[41] = "SROI"

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

tab25surf$ZONE = factor(tab25surf$ZONE, levels = c("AR7W1","NROI","AR7W2","CROI","AR7W3","SROI"))

basemap(c(-60,-47,50,62.5),bathymetry = TRUE, rotate = TRUE) +
  ggspatial::geom_spatial_point(data = tab25surf,
                                aes(x = LONGITUDE, y = LATITUDE,col= ZONE),size = 4) +
  theme(legend.key = element_blank()) +
  xlab(expression(paste("Longitude ("^o,"W)"))) +
  ylab(expression(paste("Latitude ("^o,"N)"))) +
  theme(text = element_text(size = 14),plot.background = element_rect(colour = "white"),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.key = element_rect(fill = "black"),
        axis.text = element_text(colour = "black",size=12))
ggsave("MapSamples.png",height = 8,width = 6, dpi = 600)

taxonlysurf = tab25surf %>% select(STATION,pctdiatom:pctchloro)

taxosurf.m <- melt(taxonlysurf,id.vars = "STATION")

taxosurf.m$STATION <- factor(taxosurf.m$STATION, levels = unique(taxonlysurf$STATION))
tab25surf$STATION <- factor(tab25surf$STATION, levels = unique(taxonlysurf$STATION))

colval <- c("dodgerblue2","darkorange4","palegreen2","khaki2","orchid1","green4","#FF7F00","#E31A1C","#777777")
  
SiFon = 20
gp1 <- ggplot(tab25surf,aes(x = STATION, y = CHLA, group = 1))  + geom_line(col = "#339900", linewidth = 1.2) + 
  geom_point(col = "#339900",size = 2) + scale_y_continuous(breaks = c(0,5,10),labels = c("0","5","10")) +
  xlab("") + ylab("Chl-a") + theme(axis.text.x = element_blank(),text = element_text(size = SiFon)) 
#ggplot(tab25surf,aes(x = STATION, y = 1, col = ZONE)) + geom_point()

gp2 <- ggplot(taxosurf.m,aes(x = STATION, y = value*100, fill = variable)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",text = element_text(size = SiFon)) + guides(fill = guide_legend(nrow = 1)) + 
  ylab("Percent contritubion to Chl-a")  + scale_fill_manual(name = "Group",
        values = colval,
        labels = c("Diatom", "Dino","Phaeocystis","Prasino","Crypto","Hapto","Dictyo","Chloro") )

ggarrange(gp1,gp2,ncol = 1,heights = c(1,4), align = "v")
ggsave("Transect_HPLC-Taxo_surf.png",width = 16, height = 9.5,dpi = 300)

## Surface nutrients:
ggplot() + #geom_line(col = "#339900", linewidth = 1.2) + 
  geom_point(data = tab25surf,aes(x = STATION, y = NITRATE, group = 1),col = "#339900",size = 4) +
  geom_point(data = tab25surf,aes(x = STATION, y = SILICATE, group = 1),col = "#E31A1C",size = 4) +
  geom_point(data = tab25surf,aes(x = STATION, y = PHOSPHATE*15, group = 1),col = "darkblue",size = 4) +
  scale_y_continuous(name = "Nitrate & Silicate",
                     sec.axis = sec_axis( trans = ~ . / 15, name = "Phosphate")) +
   theme(text = element_text(size = SiFon),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("Transect_Nutrient_surf.png",width = 16, height = 7,dpi = 300)

ggplot() + #geom_line(col = "#339900", linewidth = 1.2) + 
  geom_point(data = tab25surf,aes(x = NITRATEALL, y = SILICATE, colour = as.factor(ZONE)),size = 4) +
  theme(text = element_text(size = SiFon),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_abline(slope = 1,intercept = 0,linewidth = 1)

tab25surf$NITRATEALL = apply(tab25surf %>% select(NITRATE,NITRITE,AMMONIA),1,sum,na.rm = T)
ggplot(tab25surf,aes(x = diatomC4, y = NITRATEALL, colour = as.factor(ZONE))) + geom_point(size=3) +
  scale_x_log10()
ggplot(tab25surf,aes(x = diatomC4, y = SILICATE, colour = as.factor(ZONE))) + geom_point(size=3) +
  scale_x_log10()
ggplot(tab25surf,aes(x = diatomC4, y = PHOSPHATE, colour = as.factor(ZONE))) + geom_point(size=3) +
  scale_x_log10()

ggplot(tab25surf,aes(x = phaeoC4, y = NITRATEALL, colour = as.factor(ZONE))) + geom_point(size=3) +
  scale_x_log10()
ggplot(tab25surf,aes(x = phaeoC4, y = SILICATE, colour = as.factor(ZONE))) + geom_point(size=3) +
  scale_x_log10()
ggplot(tab25surf,aes(x = phaeoC4, y = PHOSPHATE, colour = as.factor(ZONE))) + geom_point(size=3) +
  scale_x_log10() +  labs(color = "Sampling")

ggplot(tab25surf,aes(x = diatomC4/CHLA, y = phaeoC4/CHLA, colour = as.factor(ZONE))) + geom_point(size=3) +
  labs(color = "Horsepower (hp)")
#CDOM:
ggplot() + #geom_line(col = "#339900", linewidth = 1.2) + 
  geom_point(data = tab25surf,aes(x = STATION, y = CDOMslope275295, colour = as.factor(ZONE)),size = 4) +
  theme(text = element_text(size = SiFon),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() + #geom_line(col = "#339900", linewidth = 1.2) + 
  geom_point(data = tab25surf,aes(x = CDOMwv350nm, y = CDOMslope275295, colour = as.factor(ZONE)),size = 4) +
  theme(text = element_text(size = SiFon),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

tab25deep <- tab %>% filter(YEAR >= 2025 & DEPTH > 20 & DEPTH <= 80)
tab25deep <- tab25deep %>% mutate(
  pctdiatom = diatomC4/CHLA,
  pctdino = dino1C4/CHLA,
  pctphaeo = phaeoC4/CHLA,
  pctprasino = prasino2C4/CHLA,
  pctcrypto = cryptoC4/CHLA,
  pcthapto = hapto6C4/CHLA,
  pctdictyo = dictyoC4/CHLA,
  pctchloro = chloroC4/CHLA,
  pctother = otherPhyto/CHLA
)


taxonlydeep = tab25deep %>% select(STATION,pctdiatom:pctchloro)

taxodeep.m <- melt(taxonlydeep,id.vars = "STATION")

taxodeep.m$STATION <- factor(taxodeep.m$STATION, levels = unique(taxodeep.m$STATION))

gp1 <- ggplot(tab25deep,aes(x = STATION, y = CHLA, group = 1))  + geom_line(col = "#339900", linewidth = 1.2) + 
  geom_point(col = "#339900",size = 2) + scale_y_continuous(breaks = c(0,5,10),labels = c("0","5","10")) +
  xlab("") + ylab("Chl-a") + theme(axis.text.x = element_blank(),text = element_text(size = SiFon)) 
#ggplot(tab25deep,aes(x = STATION, y = 1, col = ZONE)) + geom_point()

gp2 <- ggplot(taxodeep.m,aes(x = STATION, y = value*100, fill = variable)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",text = element_text(size = SiFon)) + guides(fill = guide_legend(nrow = 1)) + 
  ylab("Percent contritubion to Chl-a")  + scale_fill_manual(name = "Group",
                                                             values = colval,
                                                             labels = c("Diatom", "Dino","Phaeocystis","Prasino","Crypto","Hapto","Dictyo","Chloro") )

ggarrange(gp1,gp2,ncol = 1,heights = c(1,4), align = "v")
ggsave("Transect_HPLC-Taxo_deep.png",width = 5, height = 9,dpi = 300)


