library(ocedata)
library(oce)
data(coastlineWorldMedium, package="ocedata")
library(marmap)

#ar7w line - neadw stations
la <- c(56.1147, 56.5450, 56.9568, 57.3775, 57.8003, 58.2158, 59.4832, 59.7440,
        59.9808, 59.0685, 58.7815)
lo <- c(-53.1142, -52.6807, -52.2390, -51.7847,-51.3437, -50.8832, -49.4660,
        -49.1693, -48.8963, -49.9507, -50.4468)



# get bathymetry data
b = getNOAA.bathy(lon1 = -71, lon2 = -30, lat1 = 50, lat2 = 75, 
                  resolution = 4)

bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)


par(mar=c(4,4,1,1),mfrow=c(1,1))
plot(coastlineWorldMedium, clongitude=-57, clatitude= 58, span=2000)


# plot bathymetry
contour(bathyLon,bathyLat,bathyZ,
        levels = c(-1000, -1500, -2000,-3000,-4500),
        lwd = c(1, 1, 2, 2, 3),
        lty = c(3, 1, 3, 1, 3),
        drawlabels = F, add = TRUE, col = 'darkgray')

# add depth legend
legend("bottomleft", seg.len = 3, cex = 0.8,
       lwd = c(1, 1, 2, 2, 3),
       lty = c(3, 1, 3, 1, 3),
       legend = c(-1000, -1500, -2000,-3000,-4500),
       col = 'darkgray', title = "Depth [m]", bg= "white")


rect(xleft = -65.5, xright = -32.2, ybottom = 50.5, ytop = 62.3, 
     border = "red", lwd = 2)
lines(lo,la,col = "red")
# lines(lo_ref,la_ref, col = "blue")

# # test that i have the right coordinates for the ar7w line; check!
# ar <- ref_neadw[ref_neadw$G2expocode=="18DL20200722",]  # from 03 script, i.e. ocads data for 2020
# lo_ref <- unique(ar$G2longitude)
# la_ref <- unique(ar$G2latitude)
