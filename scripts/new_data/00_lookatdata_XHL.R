# Extended halifax line (XHL)


# 1 wrangle data
# 2 sectiojn plot
# 3 ts diagram
# 4 ages - sf6
# 5 ages - cfc

# Glodap

d <- read.csv("https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0283442/GLODAPv2.2023_Atlantic_Ocean.csv")
d <- subset(d,G2latitude >= 55 &
              G2latitude <= 62 &
              G2longitude >= -54.5 &
              G2longitude <= -48)


plot(d$G2longitude,d$G2latitude)





