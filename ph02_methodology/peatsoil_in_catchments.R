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
library(devtools)
library(jsonlite)

source("scriptstouse/function_calc_clip.R")
source("scriptstouse/function_determine_categories.R")
source("scriptstouse/function_dissolve_categories.R")
source("scriptstouse/function_df_for_suitability_areas.R")

## 1. load data
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
wg84proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
peat <- readOGR(dsn = "sourcedata", layer = "so_peat_soils_nl")
peat <- spTransform(peat, amersproj)
peat <- gBuffer(peat, byid = TRUE, width = 0)
agrilands <- readOGR(dsn= "sourcedata", layer = "so_agriculturallands_nl")
agrilandssim <- gSimplify(agrilands, tol = 0.001)
object.size(agrilandssim)/object.size(agrilands)[1]
#agrilands <- gBuffer(agrilands, width =0, byid = TRUE) 
agrilands <- dissolvepolys(agrilandssim)
agrilands2 <- spTransform(agrilands, amersproj)
agrilands <- gBuffer(agrilands, width =0, byid = TRUE) 
writeOGR(agrilands, dsn = "interdata", layer = "agrilands", driver="ESRI Shapefile", overwrite_layer = TRUE)
catchments <- readOGR(dsn = "sourcedata", layer = "so_catchments_nl")
catchmentsdata <- catchments
catchmentsdata <- spTransform(catchmentsdata, amersproj)
catchmentsdata <- gBuffer(catchmentsdata, width = 0, byid = TRUE)
catchmentsdata <- dissolvepolys(catchmentsdata)
catchmentsdata <- gBuffer(catchmentsdata, width = 0, byid = TRUE)

## 2. find agricultural lands that lie within catchments
agrilandsincatch <- quickintersect(catchmentsdata, agrilands)
agrilandsincatch2 <- gBuffer(agrilandsincatch, width = 0, byid = TRUE)
writeOGR(agrilandsincatch2, dsn = "interdata", layer = "agrilandsincatch2", driver="ESRI Shapefile", overwrite_layer = TRUE)
gIsValid(agrilandsincatch2)

## 3. find catchments that lie within agricultural lands
catchmentsinagri <- quickintersect(agrilandsincatch2, catchmentsdata)
catchmentsinagri <- gBuffer(catchmentsinagri, width = 0, byid = TRUE)
writeOGR(catchmentsinagri, dsn = "interdata", layer = "catchmentsininagri", driver="ESRI Shapefile", overwrite_layer = TRUE)
gIsValid(catchmentsinagri)

## 4. find parts of catchments that are agricultural lands
agriparts <- gIntersection(agrilandsincatch2, catchmentsinagri, byid = TRUE, drop_lower_td = TRUE)
agriparts2 <- gBuffer(agriparts,width=0,byid=T)
writeOGR(agriparts2, dsn = "interdata", layer = "agriparts2", driver="ESRI Shapefile", overwrite_layer = TRUE)
agriparts.spdf <- spdfpeatagri(agriparts2, catchmentsinagri)
agripartsdissolved <- dissolvepolys(agriparts.spdf) 

## 5. find sizes of catchments and sizes of agricultural parts of catchments
totalsizecatch <- as.numeric(gArea(catchmentsinagri, byid = TRUE)/ 10000)
agripartsizecatch <- as.numeric(gArea(agripartsdissolved, byid = TRUE)/ 10000)

## 6. merge data into data.frame
catchments <- as.character(as.data.frame(catchmentsinagri)$NAAM)
agrisizesdataframe <- data.frame(catchments, agripartsizecatch, totalsizecatch)
agrisizesdataframe$percentage <- agripartsizecatch/totalsizecatch*100

## 7. find catchments that at least for 90 % consists out of agricultural lands
highagridataframe <- agrisizesdataframe[which(agrisizesdataframe$percentage >= 90), ]$catchments

## 8. select these catchments from the initial catchment shapefile
selectedcatchments <- catchmentsdata[highagridataframe,]
writeOGR(selectedcatchments, dsn = "interdata", layer = "catchmentsinagriculturalareas", driver="ESRI Shapefile", overwrite_layer = TRUE)
selectedcatchments

## 9. find peatlands that lie within catchments
peatlandsincatch <- quickintersect(selectedcatchments, peat)

## 10. find catchments that lie within peatlands
catchmentsinpeat <- quickintersect(peatlandsincatch, selectedcatchments)

## 11. find parts of catchments that are peatlands
peatparts <- gIntersection(peatlandsincatch, catchmentsinpeat, byid = TRUE, drop_lower_td = TRUE)
peatparts.spdf <- spdfpeatagri(peatparts, catchmentsinpeat)
peatpartsdissolved <- dissolvepolys(peatparts.spdf) 
writeOGR(peatpartsdissolved, dsn = "finaldata", layer = "peatpartscatchments", driver="ESRI Shapefile", overwrite_layer = TRUE)


## 12. find sizes of catchments and sizes of peat parts of catchments
totalsizepeatcatch <- as.numeric(gArea(catchmentsinpeat, byid = TRUE)/ 10000)
peatpartsizecatch <- as.numeric(gArea(peatpartsdissolved, byid = TRUE)/ 10000)

## 13. merge data into data.frame
selectedcatchments <- as.character(as.data.frame(catchmentsinpeat)$NAAM)
peatsizesdataframe <- data.frame(selectedcatchments, peatpartsizecatch, totalsizepeatcatch)
peatsizesdataframe$percentage <- peatpartsizecatch/totalsizepeatcatch*100

## 14. categorize peat percentage of catchment
incategories <- calccategories(peatsizesdataframe[4])
peatsizesdataframe$category <- as.character(incategories)
names(catchmentsinpeat) <- "selectedcatchments"
catchmentsinpeatwithinfo <- merge(catchmentsinpeat, peatsizesdataframe, all.x = TRUE) 
catchmentsinpeatwithinfo <- spTransform(catchmentsinpeatwithinfo, wg84proj)
writeOGR(obj=catchmentsinpeatwithinfo, dsn="finaldata", layer="catchmentsinpeat", driver="ESRI Shapefile", overwrite_layer = TRUE)
