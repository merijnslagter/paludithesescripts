setwd("..")
wd <- setwd("..")
setwd(wd)

##Packages load
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)

## 1. Use so_soil_types_nl for extracting all peat areas, in R
peatsoils <- readOGR(dsn = "sourcedata", layer = "so_peat_soils_nl")

## 2. Make map of peat area with indices
index <- read.csv(file = "sourcedata/peattypesindexed.csv", check.names=FALSE)
levels(peatsoils$level) <- as.numeric(index$index)
for (i in 1:length(peatsoils[,1])){
  peatsoils$level[i]  <-  as.double(index$index[which(as.character(peatsoils$BODEM1[i]) == as.character(index$code))])
  }

writeOGR(obj=peatsoils, dsn="finaldata", layer="peatdecompvulnerability" , driver="ESRI Shapefile", overwrite_layer = TRUE)