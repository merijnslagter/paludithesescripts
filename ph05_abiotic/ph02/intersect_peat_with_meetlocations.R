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

source("scriptstouse/function_calc_clip.R")

## 00. Projections
wg84proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"

## 01. load peat data, dissolve it and buffer it
peat <- readOGR(dsn = "sourcedata", layer = "so_peat_soils_nl")
peatamer <- spTransform(peat, amersproj)
peatdissolved <- gUnaryUnion(peatamer)
peatbuffer <- gBuffer(peatdissolved, width = 300)
peatbuffer2 <- gBuffer(peatbuffer, width = 300)
peatbuffer3 <- gBuffer(peatbuffer2, width = 300)
peatbuffer4 <- gBuffer(peatbuffer3, width = 100)
datatofill <- "peatbuffer"
datatofill.df <- data.frame(datatofill)
row.names(datatofill.df) <- datatofill
row.names(peatbuffer4) <- datatofill
peatbufferspdf <- SpatialPolygonsDataFrame(peatbuffer4, datatofill.df)
peatbuffertransformed <- spTransform(peatbufferspdf, wg84proj)
writeOGR(peatbuffertransformed, "interdata", "peatbuffered1000", driver = "ESRI Shapefile", overwrite_layer = T)

## 01a. load peatbufferspdf if already made before:
peatbuffertransformed <- readOGR("interdata", "peatbuffered1000")
peatbufferspdf <- spTransform(peatbuffertransformed, amersproj)

## 02. load abiotic requirement point data and keep only those that lie within peaty areas
pH <- readOGR(dsn = "sourcedata", layer = "pH")
pHamer <- spTransform(pH, amersproj)
pHintersected <- quickintersect(peatbufferspdf, pHamer)
pHwithinpeat <-spTransform(pHintersected, wg84proj)
writeOGR(pHwithinpeat, "finaldata", "pHwithinpeat", driver = "ESRI Shapefile", overwrite_layer = T)

N <- readOGR(dsn = "sourcedata", layer = "N")
Namer <- spTransform(N, amersproj)
Nintersected <- quickintersect(peatbufferspdf, Namer)
Nwithinpeat <-spTransform(Nintersected, wg84proj)
writeOGR(Nwithinpeat, "finaldata", "Nwithinpeat", driver = "ESRI Shapefile", overwrite_layer = T)

P <- readOGR(dsn = "sourcedata", layer = "P")
Pamer <- spTransform(P, amersproj)
Pintersected <- quickintersect(peatbufferspdf, Pamer)
Pwithinpeat <-spTransform(Pintersected, wg84proj)
writeOGR(Pwithinpeat, "finaldata", "Pwithinpeat", driver = "ESRI Shapefile", overwrite_layer = T)

## 03. find number of measurement locations in total and within peat
pHnum <- length(pH[,1])
pHpeatnum <- length(pHwithinpeat[,1])
Nnum <- length(N[,1])
Npeatnum <- length(Nwithinpeat[,1])
Pnum <- length(P[,1])
Ppeatnum <- length(Pwithinpeat[,1])

namesfordf <- as.character(list("pH", "N", "P"))
numfordf <- as.character(list(pHnum, Nnum, Pnum))
peatnumfordf <- as.character(list(pHpeatnum, Npeatnum, Ppeatnum))

abioticparameters <- data.frame(namesfordf,numfordf,peatnumfordf)
names(abioticparameters) <- c("Parameters","# of measurement locations"," # of measurement locations in peat areas")

export(abioticparameters, "finaldata/abioticparametermeasurementlocations.csv")