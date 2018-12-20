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

peatincatchnl <- function(df){
  df <- as.data.frame(df)
  Veryhigh <- length(df[which(df[5]=="Very High"), ][5][,1])
  High <- length(df[which(df[5]=="High"), ][5][,1])
  Verylow <- length(df[which(df[5]=="Very Low"), ][5][,1])
  Low <- length(df[which(df[5]=="Low"), ][5][,1])
  numcatchments <- c(Veryhigh,High,Verylow,Low)
  number_of_catchments <- c(Veryhigh,High,Verylow,Low,sum(numcatchments))
  category <- c("Very High", "High", "Very Low", "Low", "Total")
  percentage <- signif(number_of_catchments/sum(numcatchments)*100,4)
  catchnumcatdf <- data.frame(category, number_of_catchments, percentage)
  return(catchnumcatdf)
  }