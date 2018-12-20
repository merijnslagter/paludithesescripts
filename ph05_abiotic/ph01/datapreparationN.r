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
parameter <- c("Parameter.code", "Parameter.omschrijving","Chemischestof.casnr")
value <- c("Numeriekewaarde", "Alfanumeriekewaarde", "Kwaliteitsoordeel.code")
combinationpossibilities <- data.frame(parameter,value)
join.df <- data.frame("","","","")
names(join.df) <- c("id1", "id2", "name", "value")

for (i in 1:length(combinationpossibilities[,1])){
  metingenonlyN <- metingen[which(metingen[[as.character(parameter[i])]] == "Ntot"),]
  metingenonlyNclean <- metingenonlyN[c("Meetpunt.DB_ID","Meetpunt.identificatie", "Meetpunt.omschrijving", as.character(value[i]))]
  names(metingenonlyNclean) <- c("id1", "id2", "name", "value")
  join.df <- rbind2(metingenonlyNclean, join.df) }
  
metingenonlyNclean <- join.df  
metingenonlyNclean$averagevalue <- 0
export(metingenonlyNclean, "interdata/metingenonlyN.csv")
idunique <- unique(metingenonlyNclean[["id1"]])
meanN <- list()
for (i in 1:length(idunique)){  
    meanN[i] <- mean(as.numeric(as.character(metingenonlyNclean[which(metingenonlyNclean$id1 == idunique[i]),]$value)))
    }
meanN <- as.character(meanN)
uniqueNdf <- data.frame(idunique, meanN)
for (i in 1:length(metingenonlyNclean[,1])){
  for(j in 1:length(idunique)){
    if (metingenonlyNclean$id1[i] == idunique[j]){
      metingenonlyNclean$averagevalue[i] <- meanN[j]
      }}}
export(metingenonlyNclean, "interdata/metingenonlyNclean.csv")      
onlyNaverage <- metingenonlyNclean[-4]
duplicatedrowsremoved <- onlyNaverage[which(!duplicated(onlyNaverage$id1)),]
duplicatedrowsremoved[which(duplicated(duplicatedrowsremoved$id2)),]
duplerows <- duplicatedrowsremoved[which(duplicated(duplicatedrowsremoved$id2)),]$id1
levels <- levels(duplicatedrowsremoved$id2)
levels <- append(levels,duplerows)
levels(duplicatedrowsremoved$id2) <- levels
for (i in 1:length(duplerows)){
  duplicatedrowsremoved$id2[which(as.character(duplicatedrowsremoved$id1) == duplerows[i])] <- duplerows[i]
  }
export(duplicatedrowsremoved,"interdata/Ndata.csv")

## 01. work on meetpunten table
meetpunten <- read.csv("sourcedata/meetpunten_2013.csv")
levels <- levels(meetpunten$Code)
levels <- append(levels,duplerows)
levels(meetpunten$Code) <- levels

for(i in 1:length(duplerows)){
  meetpunten$Code[which(meetpunten$Code == duplerows[i])] <- duplerows[i]
  }
meetpunten <- meetpunten[-6:-7]
names(meetpunten) <- c("id2","name", "waterschap", "x", "y")

## 02. take meetpunten which have a N value and merge the dataframe
joinedmeetpunten <- inner_join(duplicatedrowsremoved, meetpunten,)
summary(duplicated(joinedmeetpunten$name))
export(joinedmeetpunten, "interdata/meetpuntenjoinedN.csv")

