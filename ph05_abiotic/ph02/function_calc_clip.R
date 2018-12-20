##Packages load
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(rio)

quickintersect <- function(sp1tokeep, sp2){
  sp2.sub <- gIntersects(sp2, sp1tokeep, byid=TRUE) # test for areas that don't intersect
  sp2.sub2 <- apply(sp2.sub, 2, function(x) {sum(x)}) # test across all provincepeatgons in the SpatialPolygon whether it intersects or not
  sp2.sub3 <- sp2[which(sp2.sub2 > 0), ] # keep only the ones that actually intersect
  #intsected <- gIntersection(sp2.sub3, sp1tokeep, byid = TRUE, drop_lower_td = TRUE) 
  return(sp2.sub3)}

  
spdfpeatagri <- function(df, spdf){
  row.names(df) <-   sub(".*? (.+)", "\\1", row.names(df))
  keep <- row.names(df)
  df <- spChFIDs(df, keep)
  data <- as.data.frame(spdf@data[keep, ])
  df <- SpatialPolygonsDataFrame(df, data, match.ID = FALSE)
  return(df)
  }