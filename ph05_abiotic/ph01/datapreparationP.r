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
  metingenonlyP <- metingen[which(metingen[[as.character(parameter[i])]] == "Ptot"),]
  metingenonlyPclean <- metingenonlyP[c("Meetpunt.DB_ID","Meetpunt.identificatie", "Meetpunt.omschrijving", as.character(value[i]))]
  names(metingenonlyPclean) <- c("id1", "id2", "name", "value")
  join.df <- rbind2(metingenonlyPclean, join.df) }
  
metingenonlyPclean <- join.df  
metingenonlyPclean$averagevalue <- 0
export(metingenonlyPclean, "interdata/metingenonlyP.csv")
idunique <- unique(metingenonlyPclean[["id1"]])
meanP <- list()
for (i in 1:length(idunique)){  
    meanP[i] <- mean(as.numeric(as.character(metingenonlyPclean[which(metingenonlyPclean$id1 == idunique[i]),]$value)))
    }
meanP <- as.character(meanP)
uniquePdf <- data.frame(idunique, meanP)
for (i in 1:length(metingenonlyPclean[,1])){
  for(j in 1:length(idunique)){
    if (metingenonlyPclean$id1[i] == idunique[j]){
      metingenonlyPclean$averagevalue[i] <- meanP[j]
      }}}
export(metingenonlyPclean, "interdata/metingenonlyPclean.csv")      
onlyPaverage <- metingenonlyPclean[-4]
duplicatedrowsremoved <- onlyPaverage[which(!duplicated(onlyPaverage$id1)),]
duplicatedrowsremoved[which(duplicated(duplicatedrowsremoved$id2)),]
duplerows <- duplicatedrowsremoved[which(duplicated(duplicatedrowsremoved$id2)),]$id1
levels <- levels(duplicatedrowsremoved$id2)
levels <- append(levels,duplerows)
levels(duplicatedrowsremoved$id2) <- levels
for (i in 1:length(duplerows)){
  duplicatedrowsremoved$id2[which(as.character(duplicatedrowsremoved$id1) == duplerows[i])] <- duplerows[i]
  }
export(duplicatedrowsremoved,"interdata/Pdata.csv")

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

## 02. take meetpunten which have a P value and merge the dataframe
joinedmeetpunten <- inner_join(duplicatedrowsremoved, meetpunten,)
summary(duplicated(joinedmeetpunten$name))
export(joinedmeetpunten, "finaldata/meetpuntenjoinedP.csv")










## 0. work on metingen table
metingen <- read.csv("sourcedata/2013-1/Tijdwaarden_2013-1.csv")
metingenonlyPtot <- metingen[which(metingen$Parameter.omschrijving == "Ptot"),]
metingenonlyPtotclean <- metingenonlyPtot[c("Meetpunt.DB_ID","Meetpunt.identificatie", "Meetpunt.omschrijving", "Alfanumeriekewaarde")]
names(metingenonlyPtotclean) <- c("id1", "id2", "name", "value")
metingenonlyPtotclean$averagevalue <- 0
export(metingenonlyPtot, "interdata/metingenonlyPtot.csv")
idunique <- unique(metingenonlyPtotclean[["id1"]])
meanPtot <- list()
for (i in 1:length(idunique)){  
    meanPtot[i] <- mean(as.numeric(as.character(metingenonlyPtotclean[which(metingenonlyPtotclean$id1 == idunique[i]),]$value)))
    }
meanPtot <- as.character(meanPtot)
uniquePtotdf <- data.frame(idunique, meanPtot)
for (i in 1:length(metingenonlyPtotclean[,1])){
  for(j in 1:length(idunique)){
    if (metingenonlyPtotclean$id1[i] == idunique[j]){
      metingenonlyPtotclean$averagevalue[i] <- meanPtot[j]
      }}}
export(metingenonlyPtotclean, "interdata/metingenonlyPtotclean.csv")      
onlyPtotaverage <- metingenonlyPtotclean[-4]
duplicatedrowsremoved <- onlyPtotaverage[which(!duplicated(onlyPtotaverage$id1)),]
duplicatedrowsremoved[which(duplicated(duplicatedrowsremoved$id2)),]
levels <- levels(duplicatedrowsremoved$id2)
levels <- append(levels,c("113199", "113201"))
levels(duplicatedrowsremoved$id2) <- levels
duplicatedrowsremoved$id2[which(duplicatedrowsremoved$id1 == 113199)] <- "113199"
duplicatedrowsremoved$id2[which(duplicatedrowsremoved$id1 == 113201)] <- "113201"
export(duplicatedrowsremoved,"interdata/Ptotdata.csv")

## 01. work on meetpunten table
meetpunten <- read.csv("sourcedata/meetpunten_2013.csv")
levels <- levels(meetpunten$Code)
levels <- append(levels,c("113199", "113201"))
levels(meetpunten$Code) <- levels
meetpunten$Code[which(meetpunten$Code == "240103")] <- "113199"
meetpunten$Code[which(meetpunten$Code == "240120")] <- "113201"
meetpunten <- meetpunten[-6:-7]
names(meetpunten) <- c("id2","name", "waterschap", "x", "y")

## 02. take meetpunten which have a Ptot value and merge the dataframe
joinedmeetpunten <- inner_join(duplicatedrowsremoved, meetpunten,)
summary(duplicated(joinedmeetpunten$name))
export(joinedmeetpunten, "finaldata/meetpuntenjoinedPtot.csv")


