setwd("..")
wd <- setwd("..")
setwd(wd)

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(gridExtra)
# packages for manipulation & visualization
suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})
library(automap)
library(gdalUtils)


## coordinate systems
wg84proj <- "+proj=utm +ellps=WGS84 +datum=WGS84"
wg84 <- "+proj=longlat +datum=WGS84 +no_defs"
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"

## Load data
setwd("sourcedata")
load("pixelgrid.rda") 
setwd("..")
peatmin <- readOGR("sourcedata","peatdecompvulnerability")
peatmin <- spTransform(peatmin, amersproj)
grid.df <- data.frame(grid@coords)
names(grid.df ) <- c("x","y")
coordinates(grid.df ) <-  ~ x + y
rastergrid <- rasterFromXYZ(grid.df )
rastergrid@crs <- CRS(amersproj)
values(rastergrid) <- NA
rastergrid <- projectRaster(rastergrid, crs = amersproj)

## Rasterize
writeRaster(rastergrid,"finaldata/rastergrid.tif", datatype='FLT4S', overwrite=TRUE)
peatmin$numlev <- as.numeric(as.character(peatmin$level))
peatmin <- peatmin[which(peatmin$numlev < 5),]
peatminraster <- rasterize(peatmin, rastergrid, field = peatmin@data[,3], fun = "mean", update = TRUE, updateValue = "NA")

writeRaster(peatminraster, "finaldata/peatminraster.tif", datatype='FLT4S', overwrite=TRUE)

