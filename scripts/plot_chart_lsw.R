library(ocedata)
library(oce)
data(coastlineWorldMedium, package="ocedata")
library(marmap)

# get bathymetry data
b = getNOAA.bathy(lon1 = -71, lon2 = -30, lat1 = 50, lat2 = 75, 
                  resolution = 4)

bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)


par(mar=c(4,4,1,1),mfrow=c(1,1))
plot(coastlineWorldMedium, clongitude=-57, clatitude= 61.5, span=4000)


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



