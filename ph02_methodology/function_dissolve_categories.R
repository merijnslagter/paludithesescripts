library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)
library(maptools)
library(cleangeo)

dissolvepolys <- function(df){
  names(df)  <- "NAAM"
  row.names(df) = make.names(1:length(df), unique=TRUE)
  df <- spChFIDs(df, row.names(df))
  df <- unionSpatialPolygons(df, df@data$NAAM)
  dfdata <- data.frame(NAAM = (sapply(df@polygons, function(x) x@ID)))
  rownames(dfdata) <- dfdata[["NAAM"]]
  df <- SpatialPolygonsDataFrame(df, dfdata, match.ID = FALSE)
  return(df)
  }