library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)
library(maptools)
library(cleangeo)

calccategories <- function(table){
  levellist <- list()
  for (i in 1:length(table[,1])){
    if (table[i,1] <= 25){
      levellist[i] <- "Very Low"}
    if (table[i,1] >= 75 & table[i,1] < 101){
      levellist[i] <- "Very High"}
    if (table[i,1] >= 25 & table[i,1] < 50){
      levellist[i] <- "Low"}
    if  (table[i,1] <= 75 & table[i,1] >= 50){
      levellist[i] <- "High"}}
    return(levellist)
      }

