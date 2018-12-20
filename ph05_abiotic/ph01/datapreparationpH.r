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

## 0. work on metingen table
metingen <- read.csv("sourcedata/2013-1/Tijdwaarden_2013-1.csv")
metingenonlyunit <- metingen[which(metingen$Grootheid.code == "pH"),]
metingenonlyunitclean <- metingenonlyunit[c("Meetpunt.DB_ID","Meetpunt.identificatie", "Meetpunt.omschrijving", "Numeriekewaarde")]
names(metingenonlyunitclean) <- c("id1", "id2", "name", "value")
metingenonlyunitclean$averagevalue <- 0
export(metingenonlyunit, "interdata/metingenonlyph.csv")
idunique <- unique(metingenonlyunitclean[["id1"]])
meanunit <- list()
for (i in 1:length(idunique)){  
    meanunit[i] <- mean(as.numeric(as.character(metingenonlyunitclean[which(metingenonlyunitclean$id1 == idunique[i]),]$value)))
    }
meanunit <- as.character(meanunit)
uniqueunitdf <- data.frame(idunique, meanunit)
for (i in 1:length(metingenonlyunitclean[,1])){
  for(j in 1:length(idunique)){
    if (metingenonlyunitclean$id1[i] == idunique[j]){
      metingenonlyunitclean$averagevalue[i] <- meanunit[j]
      }}}
export(metingenonlyunitclean, "interdata/metingenonlyphclean.csv")      
onlyunitaverage <- metingenonlyunitclean[-4]
duplicatedrowsremoved <- onlyunitaverage[which(!duplicated(onlyunitaverage$id1)),]
duplicatedrowsremoved[which(duplicated(duplicatedrowsremoved$id2)),]
levels <- levels(duplicatedrowsremoved$id2)
levels <- append(levels,c("113199", "113201"))
levels(duplicatedrowsremoved$id2) <- levels
duplicatedrowsremoved$id2[which(duplicatedrowsremoved$id1 == 113199)] <- "113199"
duplicatedrowsremoved$id2[which(duplicatedrowsremoved$id1 == 113201)] <- "113201"
export(duplicatedrowsremoved,"interdata/phdata.csv")

## 01. work on meetpunten table
meetpunten <- read.csv("sourcedata/meetpunten_2013.csv")
levels <- levels(meetpunten$Code)
levels <- append(levels,c("113199", "113201"))
levels(meetpunten$Code) <- levels
meetpunten$Code[which(meetpunten$Code == "240103")] <- "113199"
meetpunten$Code[which(meetpunten$Code == "240120")] <- "113201"
meetpunten <- meetpunten[-6:-7]
names(meetpunten) <- c("id2","name", "waterschap", "x", "y")

## 02. take meetpunten which have a unit value and merge the dataframe
joinedmeetpunten <- inner_join(duplicatedrowsremoved, meetpunten,)
summary(duplicated(joinedmeetpunten$name))
export(joinedmeetpunten, "interdata/meetpuntenjoinedph.csv")

