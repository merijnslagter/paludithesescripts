setwd("..")
wd <- setwd("..")
setwd(wd)

## Packages load
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)

# main type: koopveen, madeveen etc.
# sub type: b, z, s etc.

## 00. Calculate whether main have similar indice values
peatstoutorigin <- read.csv("sourcedata/peatcodesindicesstout.csv")
peatstout <- peatstoutorigin
maintypes <- unique(peatstout$LETTER)
forboxplotmain <- list()
for (i in 1:length(maintypes)){
   indices <- peatstout[which(peatstout$LETTER == as.character(maintypes[i])),]$index
   forboxplotmain[i] <- list(indices)}
pdf('finaldata/boxplotmain.pdf')   
boxplot(forboxplotmain, names = maintypes,xlab="Main peat type", ylab="Index" )
dev.off()
sds <- list()
for (i in 1:length(forboxplotmain)){
  sds[i] <- sd(data.frame(forboxplotmain[i])[,1])}
  

## 01. Calculate Averages per main type 
averages <- data.frame(maintypes)
averages$average <- 0
for (i in 1:length(maintypes)){
  averages$average[i] <- mean(peatstout[which(peatstout$LETTER == maintypes[i]),]$index)
  }
export(data.frame(averages, as.numeric(sds)),"finaldata/sdsmains.csv")

## 02. Calculate SD's of all peat soils found
subtypes <- unique(peatstout$CYFER)
peatstout$sd <- 0

for (i in 1:length(maintypes)){
  for (j in 1:length(peatstout[which(peatstout$LETTER == maintypes[i]),]$index)){
    peatstout[which(peatstout$LETTER == maintypes[i]),]$sd[j] <- 
	peatstout[which(peatstout$LETTER == maintypes[i]),]$index[j] - averages$average[i]
      }}
      
## 02b. Calculate whether subtypes have similar qualities
forboxplotsub <- list()
for (i in 1:length(subtypes)){
   indices <- peatstout[which(peatstout$CYFER == subtypes[i]),]$sd
   forboxplotsub[i] <- list(indices)}
pdf('finaldata/boxplotsub.pdf')   
boxplot(forboxplotsub, names = subtypes, xlab="Sub peat type", ylab="Derivation from main index" )
dev.off()
sdssd <- list()
for (i in 1:length(forboxplotsub)){
  sdssd[i] <- sd(data.frame(forboxplotsub[i])[,1])}

export(data.frame(sds, as.numeric(sdssd)),"finaldata/sdssubs.csv")
      
## 03. Calculate average SD per subtype       
sds <- data.frame(subtypes)
sds$SD <- 0
for (i in 1:length(subtypes)){
  sds$SD[i] <- mean(peatstout[which(peatstout$CYFER == subtypes[i]),]$sd)
    }
    
## 04. Calculate average indices for waardveengronden and veenkoloniale gronden
veenkoloniaal <- read.csv("sourcedata/calcveenkoloniaalindex.csv")
for (i in 1:length(veenkoloniaal[,1])){
  veenkoloniaal$b_c[i] <- veenkoloniaal$b[i] * veenkoloniaal$c[i]
  }
profileindexveenkoloniaal <- 0.2
maximumvalueallindices <- 2.2768
finalindexveenkoloniaal <- (sum(veenkoloniaal$b_c)*profileindexveenkoloniaal) / maximumvalueallindices
waardveen  <-  read.csv("sourcedata/calcwaardveenindex.csv")
for (i in 1:length(waardveen[,1])){
  waardveen$b_c[i] <- waardveen$b[i] * waardveen$c[i]
  }
profileindexwaardveen <- 0.6
finalindexwaardveen <- (sum(waardveen$b_c)*profileindexwaardveen) / maximumvalueallindices

## 05. add waardveen and koloniale gronden to maintype average list
maintypes <- c("kV", "iV")
average <- c(finalindexwaardveen, finalindexveenkoloniaal)
averageaddition <- data.frame(maintypes,average)
averages <- rbind(averages,averageaddition)

## 06. calculate sd for bagger, verslagen veen and gyttja (d) by averaging the sd for s,b,c and r:  
sdgyttja <- mean(c(sds[1,2],sds[4,2],sds[6,2],sds[7,2]))
subtypes <- "d"
SD <- sdgyttja
SDaddition <- data.frame(subtypes,SD)
sds <- rbind(sds,SDaddition)

## 07. Calculate indices for missing peat soil types
missing <- read.csv("sourcedata/peatcodesindicesmissing.csv")
maintypesmissing <- unique(missing$LETTER)
subtypesmissing <- unique(missing$CYFER)
levels(missing$LETTER) <- averages$maintypes[-5]
levels(missing$CYFER) <- levels(sds$subtypes)
for (i in 1:length(missing[,1])){
  missing$index[i] <- averages$average[which(averages$maintypes %in% missing$LETTER[i])] + sds$SD[which(sds$subtypes %in% missing$CYFER[i])]
	}
extrapolated <- rbind(peatstoutorigin,missing)

## 08. find out which codes are already been indexed but where not taken into account previously
stouthameroriginal <- read.csv("sourcedata/stouthameroriginindex.csv")
a <- stouthameroriginal[,1]
b <- extrapolated[,1]
duplicates <- Reduce(intersect, list(a,b))
stouthamerleftover <- stouthameroriginal[which(!stouthameroriginal$code %in% duplicates),]
export(stouthamerleftover, "interdata/stouthamerleftover.csv")
stouthamerleftoverwithnames <- read.csv("interdata/stouthamerleftoverwithnames.csv")
extrapolated <- extrapolated[-2:-3]
extrapolated$index <- as.character(extrapolated$index)
stouthamerleftoverwithnames$index <- as.character(stouthamerleftoverwithnames$index)
indexes <- rbind(stouthamerleftoverwithnames, extrapolated)
export(indexes, "interdata/indexedstouthamerandextrapolation.csv")

## 08. Find out what codes are available in the soil map and are not yet indexed
soiltypesource <- readOGR(dsn = "sourcedata", layer = "bodem50")
soiltype <- soiltypesource
soiltype <- soiltype["BODEM1"]
soiltypelist <- unique(as.data.frame(soiltype))
peatlistv <- subset(soiltypelist, grepl("V", soiltypelist[,1]))
peatlistw <- subset(soiltypelist, grepl("W", soiltypelist[,1]))
peatlist <- full_join(peatlistv, peatlistw)
peatlist <- subset(peatlist, ((nchar(as.character(peatlist[["BODEM1"]])) <= 4)))
duplicates <- Reduce(intersect, list(peatlist[,1],indexes[,1]))
peattypestobeindexed <- data.frame(peatlist[which(!peatlist$BODEM1 %in% duplicates),])
export(peattypestobeindexed, "interdata/manuallyaddindexes.csv")
peattypesindexed <- read.csv("interdata/manuallyaddindexesdone.csv")
indexes <- rbind(indexes,peattypesindexed)
export(indexes, "finaldata/peattypesindexed.csv")
