setwd("..")
wd <- setwd("..")
setwd(wd)

##Packages load
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)
library(maptools)
library(cleangeo)
library(jsonlite)
library(data.table)

wg84proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"

##  Ntot
table <- read.csv("interdata/meetpuntenjoinedN.csv", header = T, sep = ",")
coordinates(table) <- ~x+y
proj4string(table) = amersproj
table <- spTransform(table, wg84proj)
writeOGR(table, "finaldata", "N", driver = "ESRI Shapefile", overwrite_layer = T)


## Ptot
table <- read.csv("interdata/meetpuntenjoinedP.csv", header = T, sep = ",")
coordinates(table) <- ~x+y
proj4string(table) = amersproj
table <- spTransform(table, wg84proj)
writeOGR(table, "finaldata", "P", driver = "ESRI Shapefile", overwrite_layer = T)

## pH
table <- read.csv("interdata/meetpuntenjoinedph.csv", header = T, sep = ",")
coordinates(table) <- ~x+y
proj4string(table) = amersproj
table <- spTransform(table, wg84proj)
writeOGR(table, "finaldata", "pH", driver = "ESRI Shapefile", overwrite_layer = T)