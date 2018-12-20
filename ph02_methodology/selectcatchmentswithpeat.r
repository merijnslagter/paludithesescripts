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

## Load data
wg84proj <- "+proj=utm +ellps=WGS84 +datum=WGS84"
wg84 <- "+proj=longlat +datum=WGS84 +no_defs"
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
units <- readOGR(dsn = "sourcedata", layer ="ahn_units_correct_crs")
units <- spTransform(units, wg84proj)
catchments <- readOGR(dsn = "sourcedata", layer ="catchmentsinpeat")
catchments <- spTransform(catchments, wg84proj)
catchments <- gBuffer(catchments, width = 0, byid = T)
line <- as(units, 'SpatialLinesDataFrame')

## Find catchments with either 10-20,20-30,30-40,40-50,50-60,60-70,70-80,80-90,90-100 % of peat in the catchment
cat1020 <- catchments[which(10 <= catchments$percntg & catchments$percntg <= 20),]
cat2030 <- catchments[which(20 <= catchments$percntg & catchments$percntg <= 30),]
cat3040 <- catchments[which(30 <= catchments$percntg & catchments$percntg <= 40),]
cat4050 <- catchments[which(40 <= catchments$percntg & catchments$percntg <= 50),]
cat5060 <- catchments[which(50 <= catchments$percntg & catchments$percntg <= 60),]
cat6070 <- catchments[which(60 <= catchments$percntg & catchments$percntg <= 70),]
cat7080 <- catchments[which(70 <= catchments$percntg & catchments$percntg <= 80),]
cat8090 <- catchments[which(80 <= catchments$percntg & catchments$percntg <= 90),]
cat90100 <- catchments[which(90 <= catchments$percntg & catchments$percntg <= 100),]

## For each category, find out which catchments lie only within the border of ahn unit
quickintersect <- function(sp1tokeep, sp2){
  sp2.sub <- gIntersects(sp2, sp1tokeep, byid=TRUE) # test for areas that don't intersect
  sp2.sub2 <- apply(sp2.sub, 2, function(x) {sum(x)}) # test across all provincepeatgons in the SpatialPolygon whether it intersects or not
  sp2.sub3 <- sp2[which(sp2.sub2 == 0), ] # keep only the ones that actually intersect
  #intsected <- gIntersection(sp2.sub3, sp1tokeep, byid = TRUE, drop_lower_td = TRUE) 
  return(sp2.sub3)}
  
cat1020noint <- quickintersect(line, cat1020)
cat2030noint <- quickintersect(line, cat2030)
cat3040noint <- quickintersect(line, cat3040)
cat4050noint <- quickintersect(line, cat4050)
cat5060noint <- quickintersect(line, cat5060)
cat6070noint <- quickintersect(line, cat6070)
cat7080noint <- quickintersect(line, cat7080)
cat8090noint <- quickintersect(line, cat8090)
cat90100noint <- quickintersect(line, cat90100)

## If no catchment is found within a ahn unit, choose one that lies within 2 ahn units
cat1020noint
cat2030noint
cat3040noint
cat4050noint
cat5060noint
cat6070noint 
cat7080noint 
cat8090noint 
cat90100noint 

## For the leftover catchments, check whether the size is bigger than 2 hectares

cat1020noint2big <- cat1020noint[which(cat1020noint$ptprtsz > 2),]
cat2030noint2big <- cat2030noint[which(cat2030noint$ptprtsz > 2),]
cat3040noint2big <- cat3040noint[which(cat3040noint$ptprtsz > 2),]
cat4050noint2big <- cat4050noint[which(cat4050noint$ptprtsz > 2),]
cat5060noint2big <- cat5060noint[which(cat5060noint$ptprtsz > 2),]
cat6070noint2big <- cat6070noint[which(cat6070noint$ptprtsz > 2),]
cat7080noint2big <- cat7080noint[which(cat7080noint$ptprtsz > 2),]
cat8090noint2big <- cat8090noint[which(cat8090noint$ptprtsz > 2),]  
cat90100noint2big <- cat90100noint[which(cat90100noint$ptprtsz > 2),]

## check whether there are leftover catchments

cat1020noint2big 
cat2030noint2big 
cat3040noint2big
cat4050noint2big 
cat5060noint2big 
cat6070noint2big 
cat7080noint2big
cat8090noint2big 
cat90100noint2big

## Randomly select catchments from list 
cat1020random <- cat1020noint2big[which(cat1020noint2big$slctdct == as.character(cat1020noint2big$slctdct[sample(1:length(cat1020noint2big$slctdct),1)])),]
cat2030random <- cat2030noint2big [which(cat2030noint2big $slctdct == as.character(cat2030noint2big $slctdct[sample(1:length(cat2030noint2big $slctdct),1)])),]
cat3040random <- cat3040noint2big[which(cat3040noint2big$slctdct == as.character(cat3040noint2big$slctdct[sample(1:length(cat3040noint2big$slctdct),1)])),]
cat4050random <- cat4050noint2big [which(cat4050noint2big $slctdct == as.character(cat4050noint2big $slctdct[sample(1:length(cat4050noint2big $slctdct),1)])),]
cat5060random <- cat5060noint2big [which(cat5060noint2big $slctdct == as.character(cat5060noint2big $slctdct[sample(1:length(cat5060noint2big $slctdct),1)])),]
cat6070random <- cat6070noint2big [which(cat6070noint2big $slctdct == as.character(cat6070noint2big $slctdct[sample(1:length(cat6070noint2big $slctdct),1)])),]
cat7080random <- cat7080noint2big[which(cat7080noint2big$slctdct == as.character(cat7080noint2big$slctdct[sample(1:length(cat7080noint2big$slctdct),1)])),]
cat8090random <- cat8090noint2big [which(cat8090noint2big $slctdct == as.character(cat8090noint2big $slctdct[sample(1:length(cat8090noint2big $slctdct),1)])),]
cat90100random <- cat90100noint2big[which(cat90100noint2big$slctdct == as.character(cat90100noint2big$slctdct[sample(1:length(cat90100noint2big$slctdct),1)])),]

## Manualy check whether catches are nicely spread
catchrandomselected <- rbind(cat1020random,cat2030random,cat3040random,cat4050random,cat5060random,cat6070random,cat7080random,cat8090random,cat90100random)
plot(catchrandomselected)

## Write as shapefile
catchrandomselected <- spTransform(catchrandomselected, wg84)
writeOGR(obj=catchrandomselected, dsn="interdata", layer="randomselectedcatchments" , driver="ESRI Shapefile", overwrite_layer = TRUE)

