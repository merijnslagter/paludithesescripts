setwd("..")
wd <- setwd("..")
setwd(wd)

##Packages load
library(rgdal)
library(rgeos)
library(dplyr)
library(rio)
library(maptools)

## create so_soil_types_nl
soiltypesource <- readOGR(dsn = "../sourcedata", layer = "bodem50")
soiltype <- soiltypesource
soiltype <- soiltype["BODEM1"]
soiltypelist <- unique(as.data.frame(soiltype))
peatlistv <- subset(soiltypelist, grepl("V", soiltypelist[,1]))
peatlistw <- subset(soiltypelist, grepl("W", soiltypelist[,1]))
peatlist <- full_join(peatlistv, peatlistw)
peatlist <- subset(peatlist, ((nchar(as.character(peatlist[["BODEM1"]])) <= 4)))
tablelist <- list()
for(i in 1:length(peatlist[,1])){  
  tablelist[[i]]  <- soiltype[as.data.frame(soiltype [,"BODEM1"])[,1] == as.character(peatlist[i,1]),1]
   }
peatsoil <- do.call(bind, tablelist) 
writeOGR(obj=peatsoil , dsn="finaldata", layer="so_peat_soils_nl" , driver="ESRI Shapefile", overwrite_layer = TRUE)

## create so_provinces_nl, delete "IJsselmeer" and "Zeeuwse meren"
provinces <- readOGR(dsn = "sourcedata", layer = "provinces")
names(provinces) <- "name"
provinces <- provinces[-6, drop = TRUE]
provinces <- provinces[-12, drop = TRUE]
writeOGR(obj=provinces , dsn="finaldata", layer="so_provinces_nl" , driver="ESRI Shapefile", overwrite_layer = TRUE)

## create so_agriculturallands_nl
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
wg84proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
lgn <- readOGR(dsn = "sourcedata", layer = "lgn2012")
lgn2012 <- lgn["BG2012"]
agri <- subset(lgn2012, grepl(51, BG2012))
agriprojected <- spTransform(agri, amersproj)
agri <- gBuffer(agriprojected, byid = TRUE, width = 0)
rm(agriprojected)
gc()

agrilandssim <- gSimplify(agri, tol = 0.0050)
agrilandssim <- gBuffer(agrilandssim, byid = TRUE, width = 0)
agrilandssim <- spTransform(agrilandssim, wg84proj)
agrilandsdis <- gUnaryUnion(agrilandssim)
testdf <- data.frame("Lala")
agrilandsdis <- SpatialPolygonsDataFrame(agrilandsdis, testdf)
agriproj <- spTransform(agri, wg84proj)

writeOGR(obj=agrilandsdis , dsn="finaldata", layer="so_agriculturallands_nl" , driver="ESRI Shapefile", overwrite_layer = TRUE)

## create so_catchments_all
frieslandsource <- readOGR(dsn = "../sourcedata/peilbesluit/friesland", layer = "Peilbesluitenkaart_WF")
frieslandsource <- frieslandsource["GPGNAAM"]
frieslandsource <- spTransform(frieslandsource, proj4string(soiltypesource))
names(frieslandsource) <- "NAAM"
hdsrsource <- readOGR(dsn = "../sourcedata/peilbesluit/hdsr", layer = "peilgebieden_veengebieden_hdsr")
hdsrsource <- hdsrsource["NAAM"]
hdsrsource <- spTransform(hdsrsource, proj4string(soiltypesource))
rijnlandsource <- readOGR(dsn = "../sourcedata/peilbesluit/Rijnland/Peilvak_en_peilafwijking_praktijk", layer = "Peilvak_en_peilafwijking_praktijk_geheel_rijnland")
rijnlandsource <- rijnlandsource["NAAM"]
rijnlandsource <- spTransform(rijnlandsource, proj4string(soiltypesource))
wsrlsource <- readOGR(dsn = "../sourcedata/peilbesluit/wsrl/peilgebieden_wsrl.shp", layer = "peilgebieden_wsrl")
wsrlsource <- wsrlsource["GPGNAAM"]
names(wsrlsource) <- "NAAM"
wsrlsource <- spTransform(wsrlsource, proj4string(soiltypesource))
agvsource <- readOGR(dsn="../sourcedata/peilbesluit/waternetAGV/Peilgebieden.gdb",layer="IngesteldePeilgebieden")
agvsource <- agvsource["NAAM"]
agvsource <- spTransform(agvsource, proj4string(soiltypesource))
peilvakkennl <- rbind(frieslandsource,hdsrsource, rijnlandsource,wsrlsource,agvsource)
peilvakkenamer <- spTransform(peilvakkennl, CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs" ))
peilvakkenb <- gBuffer(peilvakkenamer, byid = TRUE, width = 0)
writeOGR(obj=peilvakkenb, dsn="../finaldata", layer="so_catchments_nl" , driver="ESRI Shapefile", overwrite_layer = TRUE)


