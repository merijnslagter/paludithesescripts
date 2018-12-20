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
library(pastecs)
## 00. load data
ph <- readOGR("sourcedata","pHwithinpeat")
N <- readOGR("sourcedata","Nwithinpeat")
P <- readOGR("sourcedata","Pwithinpeat")

## 01. pH update
cattaillowlimit <- 5
ph$cattailgood <- "yes"
levels(ph$cattailgood) <- c("yes","no")
for (i in 1:length(ph[,1])){
  if (ph$avergvl[i] < cattaillowlimit){
    ph$cattailgood[i] <- "no"}}
peatmossuplimit <- 6.5
ph$peatmossgood <- "yes"
levels(ph$peatmossgood) <- c("yes","no")
for (i in 1:length(ph[,1])){
  if (ph$avergvl[i] > peatmossuplimit){
    ph$peatmossgood[i] <- "no"}}    
writeOGR(ph, "finaldata", "phupdated", driver = "ESRI Shapefile", overwrite_layer = T)
length(ph$peatmossgood[which(ph$peatmossgood == "yes")])
length(ph[,1])
length(ph$cattailgood[which(ph$cattailgood == "yes")])
length(ph[,1])

## 02. P update
# Very High : above 0.35
# High: between 0.25 and 0.35
# Suitable: Between 0.15 and 0.25
# Moderate: Below 0.15
P$suitable <- "Moderate"
suitrange <- c(0.15,0.25,0.35)
suitlevels <- c("Moderate suitability","Neutral suitability","High suitability","Very high suitability")
levels(P$suitable) <- suitlevels
P$suitable <- as.character(calccategories(P$avergvl,suitrange,suitlevels))
writeOGR(P, "finaldata", "Pupdated", driver = "ESRI Shapefile", overwrite_layer = T)

## 03. N update
# Very High : above 4
# High: between 3 and 4
# Suitable: Between 2 and 3
# Moderate: Below 2
suitrange <- c(2,3,4)
suitlevels <- c("Moderate suitability","Neutral suitability","High suitability","Very high suitability")
N$suitable <- "Moderate"
levels(N$suitable) <- suitlevels
N$suitable <- as.character(calccategories(N$avergvl,suitrange,suitlevels))
writeOGR(N, "finaldata", "Nupdated", driver = "ESRI Shapefile", overwrite_layer = T)
# # 
stat.desc(N$avergvl)