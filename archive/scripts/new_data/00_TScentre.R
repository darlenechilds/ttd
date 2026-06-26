# TS with ODF
rm(list = ls())

i <- 2024

# Settings for each year
settings <- list("2023" = list(cruise = "CAR2023573",casts  = c(14, 18, 23, 27, 31, 35, 40, 44)),
                 "2024" = list(cruise = "CAR2024924",casts  = c(10, 13, 18, 25, 28, 32, 37, 41)),
                 "2025" = list(cruise = "LAT2025146",casts  = c(12, 20, 25, 28, 111, 131, 134, 136, 148, 163))
)

# Pull settings for selected year
wanted.cruise <- settings[[as.character(i)]]$cruise
wanted.casts  <- settings[[as.character(i)]]$casts

files <- list.files(
  paste0("R:/Science/BIODataSvc/ARC/Archive/ctd/", i),
  pattern = "\\.ODF$",
  full.names = TRUE)

b <- basename(files)

cruise <- sub("^CTD_([^_]+)_.*", "\\1", b)
castnum <- as.numeric(sub("^CTD_[^_]+_(\\d{3})_.*", "\\1", b))
direction <- sub("^.*_(DN|UP)\\.ODF$", "\\1", b)

lsw <- files[cruise == wanted.cruise &castnum %in% wanted.casts &direction == "DN"]

##  get wanted data
dat <- NULL
for (j in lsw){
txt <- readLines(j)
i <- grep("^-- DATA --", txt)    # find start of data
d <- read.table(text = txt[(i + 1):length(txt)],fill = TRUE,quote = "'",stringsAsFactors = FALSE)
d <- cbind.data.frame(d$V4,d$V6,d$V38)
names(d) <- c("prDM","t090c", "sal00")
dat <- rbind.data.frame(dat,d)
}

head(dat)

#remove surface 
# surf_ind <- which(dat$prDM<200)
deep_ind <- which(dat$prDM>200)

ind <- which(dat$t090c<20)
# ind <- which(dat$sal00>35.2)

dat2 <- dat[ind,]

dat2 <- dat

plot(dat2$sal00,dat2$t090c, pch = 16, main = paste("PSAL vs Te90 w sf6",i))
plot(dat2$t090c,-dat2$prDM, xlim = c(3.2,3.9), ylim = c(-1500,500))
plot(dat2$sal00,-dat2$prDM)

# ,
     # ylim = c(1.0, 6),
     # xlim = c(33.5, 35))


#get sf6 data
d <- read.csv("data/processed/UNSUB_tracers_o2.csv")
d <- d[order(d$SAMPNO),]
d <- d[-which(is.na(d$SF6)),]
years <- c( "HUD2015006" = 2015," HUD2016006" = 2016, "HUD2018008" = 2018," AMU2019001" = 2019,
  "AMU2020001" = 2020,"AT4805"     = 2022,"CAR2023573" = 2023,"CAR2024924" = 2024,"LAT2025146" = 2025)
d$year <- years[d$EXPOCODE]
pal <- colorRampPalette(c("blue", "lightgrey","red"))
ncol <- 100
sf6.col <- pal(ncol)



e <- d[d$year==i,]
centre <- e[e$LATITUDE > 56 & e$LATITUDE <59.1,]
centre <- e[e$CTDPRS>200,]

col.ind <- cut(centre$SF6, breaks = ncol, labels = FALSE)
points(centre$CTDSAL,centre$CTDTMP, pch = 16, col = sf6.col[col.ind])


points(centre$CTDSAL,centre$CTDTMP, pch = 16, col = "green")
unique(e$STNNBR)
