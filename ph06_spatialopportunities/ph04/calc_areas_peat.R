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

peat <- readOGR(dsn ="sourcedata", layer = "so_peat_soils_nl")
amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
peat <- spTransform(peat, amersproj)
peatarea <- gArea(peat)/10000

## Calc scenarios reduction 

ghgscenario <- function(scenario = scenario1, percent = 10) {
  paludiarea <- peatarea *( percent/100)
  ghgnow <- paludiarea * scenario[1]
  ghgreduction1 <- paludiarea *  scenario[2]
  ghgreduction2 <- paludiarea *  scenario[3]
  reduction1 <- ghgnow - ghgreduction1
  reduction2 <- ghgnow - ghgreduction2
  totalemissions <- peatarea * scenario[1]
  reduce <- c(reduction1,reduction2, ghgnow, paludiarea, peatarea, totalemissions)
  return(reduce)
  }


scenario2 <- c(28.76, 13.39, 8.14)

reducesc2 <- ghgscenario(scenario2)

reduces20percentsc2 <- ghgscenario(scenario2, 20)
reduces30percentsc2 <- ghgscenario(scenario2, 30)

export(data.frame(reducesc2,reduces20percentsc2, reduces30percentsc2), "finaldata/ghgscenariosnational.csv")