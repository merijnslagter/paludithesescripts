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

source("scriptstouse/function_load_correct_raster_ahn.R")
source("scriptstouse/function_intersect.r")

## load data
wg84proj <- "+proj=utm +ellps=WGS84 +datum=WGS84"
wg84 <- "+proj=longlat +datum=WGS84 +no_defs"
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
units <- readOGR(dsn = "sourcedata", layer ="ahn_units_correct_crs")
catchments <- readOGR(dsn = "sourcedata", "randomselectedcatchments")
peatcatch <- readOGR(dsn ="sourcedata", layer = "peatpartscatchments")
names <- as.character(catchments$slctdct)
quantile1list <- 0
quantile9list <- 0
quantilediflist <- 0

for (i in 1:length(names)){
## find catchment elevation data per ahn unit
  catchment <- catchments[which(as.data.frame(catchments)$slctdct == names[i]),]
  unitstokeep <- quickintersect(catchment,units)
  unitstokeep <- as.character(as.data.frame(unitstokeep)$UNIT)
  
  if (!file.exists(paste("interdata/heightrasters/", names[i], ".tif" , sep = ''))){
  catchmentraster <- getahnraster(unitstokeep)
  
## crop and mask catchment elevation data for catchment
  ahncrop <- crop(catchmentraster, spTransform(catchment, proj4string(catchmentraster)), snap ="out")
  heightcropmask <- mask(ahncrop, spTransform(catchment, proj4string(catchmentraster)))
  
## Eliminate all elevations above 0 m 
  if(  quantile(heightcropmask, probs = 0.5, na.rm=TRUE) < 0){
  fun <- function(x) { x[x>0] <- NA; return(x) }
  heightcropmask <- calc(heightcropmask, fun)
  }

## write raster file catchment as .tif
  writeRaster(projectRaster(heightcropmask,crs = wg84), filename= paste("interdata/heightrasters/", names[i], ".tif" , sep = ''), datatype='FLT4S', overwrite=TRUE)
  }
  heightcropmask <- raster(paste("interdata/heightrasters/", names[i], ".tif" , sep = ''))

## calculate elevation at .1 and .9 quantile and substract these for elevation differences in catchment
  quantile1 <- quantile(heightcropmask, probs = 0.1, na.rm=TRUE)
  quantile9 <- quantile(heightcropmask, probs = 0.9, na.rm=TRUE)
  eldif <- quantile9 - quantile1
  quantile1list[i] <- quantile1
  quantile9list[i] <- quantile9
  quantilediflist[i] <- eldif
  }
export(data.frame(names, quantile1list  ,quantile9list,quantilediflist), "finaldata/eldifcatch.csv")

## Make peat only rasters of catchments
areapeat <- 0
for (i in 1:length(names)){ 
  peatincatchment <- peatcatch[which(as.data.frame(peatcatch)$NAAM == names[i]),]
  
  ## calculate size of peat area
  areapeat[i] <- gArea(peatincatchment)/10000
  heightcropmask <- raster(paste("finaldata/heightrasters/all/", names[i], ".tif" , sep = ''))
  peatcrop <- crop(projectRaster(heightcropmask, crs = proj4string(catchmentraster)), spTransform(peatincatchment, proj4string(catchmentraster)), snap = "out")
  peatcrop <- mask(projectRaster(heightcropmask, crs = proj4string(catchmentraster)), spTransform(peatincatchment, proj4string(catchmentraster)))
  writeRaster(peatcrop, filename= paste("finaldata/heightrasters/peat/", names[i], ".tif" , sep = ''), datatype='FLT4S', overwrite=TRUE)
  }

export(data.frame(names,areapeat),"finaldata/areapeatcatch.csv")
