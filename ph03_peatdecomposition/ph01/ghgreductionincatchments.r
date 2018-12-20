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
require(RCurl)

source("scriptstouse/function_co2wl.R")

## load raster file
filenamespeat <- list.files("interdata/heightrasters/peat", pattern="*.tif", full.names=TRUE)
filenameswhole <- list.files("interdata/heightrasters/all", pattern="*.tif", full.names=TRUE)

## write co2 calc function
co2calccatch <- function(ahnraster, quantile = 0.1, quantileraster, drainage = 0.4, paludi = 0.1){
    quantilechoice <- quantile(quantileraster, probs = quantile, na.rm=TRUE)
    drainage <- quantilechoice - drainage
    ahndrainage <- (drainage-ahnraster)*100
    co2drainage <- calc(ahndrainage, co2wt)
    ahnpaludi <- (quantilechoice + paludi - ahnraster)*100
    co2paludi <- calc(ahnpaludi, co2wt)
    co2paludi <- co2paludi / 40000
    co2drainage <- co2drainage / 40000
    co2paluditons <- cellStats(co2paludi, stat = 'sum')
    co2drainagetons <- cellStats(co2drainage, stat = 'sum')
    co2intons <- c(co2paluditons,co2drainagetons)
    scenario <- c("paludi", "drainage")
    return(data.frame(scenario,co2intons))
    }

## determine factors for simulations
drainage04 <- 0.4
drainage05 <- 0.5
paludi01 <- 0.1
paludi00 <- 0.0
    
drainagevar <- c(drainage04,drainage05)
paludivar <- c(paludi01,paludi00)
    
## calculate co2 emissions
co2paludi <- 0
co2drainage <- 0
co2diff <- 0
for (a in 1:2){
  for (b in 1:2){
    for (i in 1:length(filenamespeat)){
      ahnrasterwhole <- raster(filenameswhole[i])
      ahnraster <- raster(filenamespeat[i])
      co2 <- co2calccatch(ahnraster, quantileraster = ahnrasterwhole, paludi = paludivar[a], drainage = drainagevar[b])
      co2paludi[i] <- co2$co2intons[1]
      co2drainage[i] <- co2$co2intons[2]
      co2diff[i] <- co2$co2intons[2] - co2$co2intons[1]
      }
      export(data.frame(filenamespeat,co2drainage,co2paludi,co2diff), paste("finaldata/test/ghgemissions_paludi_",paludivar[a],"_drainage_",drainagevar[b],".csv", sep = ''))
      }}
      

sizepeatreal <- 0      
for (i in 1:length(filenamespeat)){
  ahnraster <- raster(filenamespeat[i])
  a <- !is.na(ahnraster)[]
  a <- length(a[which(a == TRUE)])
  areapeat <- (a * 0.25)/10000
  sizepeatreal[i] <- areapeat
  }

export(data.frame(filenamespeat, sizepeatreal), "finaldata/sizesreal.csv")  
         
         
      