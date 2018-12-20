##Packages load
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)
library(maptools)
library(cleangeo)
library(jsonlite)

getahnraster <- function(units){
  listahn <- list()
  for (i in 1:length(units)){
    upper<- toupper(as.character(units[i]))
    lower <- as.character(units[i])
    if (!file.exists(paste("interdata/ahn3/", upper, ".zip", sep = ''))){
      download.file(paste("https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dtm/M_", upper, ".ZIP", sep = ''), paste("interdata/ahn3/", upper, ".zip", sep = ''))
      unzip(paste("interdata/ahn3/", upper, ".zip", sep = '') ,exdir = "interdata/ahn3")
      }
    if(length(units) > 1){
      listahn[i] <- raster(paste("interdata/ahn3/m_", lower, ".tif", sep = ''))}
    }
    if (length(units) > 1){
      ahn <- do.call(merge, listahn) }
    if (length(units) == 1){
      ahn <- raster(paste("interdata/ahn3/m_", lower, ".tif", sep = ''))}
    return(ahn)  
  }