setwd("..")
wd <- setwd("..")
setwd(wd)

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(gridExtra)
# packages for manipulation & visualization
suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})
library(automap)

rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x)
  if(is.null(x.max)) x.max = max(x)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}

## Load data
setwd("sourcedata")
load("pixelgrid.rda") 
setwd("..")


## Write Krige function

krigewaterquality <- function(parameter, rastergrid, name, colname){
  kriged <- autoKrige(parameter[colname],new_data=rastergrid, verbose = TRUE)
  kriging.pred <- kriged$krige_output
  pred <- kriging.pred["var1.pred"]
  pred.df <- data.frame(pred@coords,pred@data)
  names(pred.df ) <- c("x","y", name)
  coordinates(pred.df ) <-  ~ x + y
  raster <- rasterFromXYZ(pred.df)
  return(raster)}

amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"


## pH load and krige
ph <- readOGR("sourcedata", "phupdated")
ph <- spTransform(ph, CRS(amersproj))
phkriged <- krigewaterquality(ph, grid, 'ph','avergvl') 

## Cattail broadleaf
# lower boundary:
lowbow <- 5
# upper boundary:
highbow <- 8

phcb <- phkriged
phcb[which(phcb[] >= lowbow & phcb[]<= highbow) ] <- 1
phcb[which(phcb[] != 1)] <- 0
writeRaster(phcb, "finaldata/phcb.tif", datatype='FLT4S', overwrite=TRUE)

## Narrowleaf broadleaf
# lower boundary:
lowbow <- 3
# upper boundary:
highbow <- 9

phcn <- phkriged
phcn[which(phcn[] >= lowbow & phcn[]<= highbow) ] <- 1
phcn[which(phcn[] != 1)] <- 0
writeRaster(phcn, "finaldata/phcn.tif", datatype='FLT4S', overwrite=TRUE)

## Peat moss 
# lower boundary:
lowbow <- 3
# upper boundary:
highbow <- 6.5

phpm <- phkriged
phpm[which(phpm[] >= lowbow  & phpm[]<= highbow) ] <- 1
phpm[which(phpm[] != 1)] <- 0
writeRaster(phpm, "finaldata/phpm.tif", datatype='FLT4S', overwrite=TRUE)

## Azolla
# lower boundary:
lowbow <- 4
# upper boundary:
highbow <- 8

phaz <- phkriged
phaz[which(phaz[] >= lowbow & phaz[]< highbow) ] <- 1
phaz[which(phaz[] != 1)] <- 0
writeRaster(phaz, "finaldata/phaz.tif", datatype='FLT4S', overwrite=TRUE)

phoneindex <- phkriged
phoneindex <- mean(phaz, phpm, phcn, phcb)
#writeRaster(phoneindex, "finaldata/phoneindex.tif", datatype='FLT4S', overwrite=TRUE)

## P 
P <- readOGR("sourcedata", "Pupdated")
Pquantile <- as.numeric(quantile(P$avergvl, probs = 0.9))
#normalize
Pscaled <- P$avergvl/Pquantile
Pscaled[which(Pscaled > 1)] <- 1
P$avergvl <- Pscaled
P <-  spTransform(P, CRS(amersproj))

Pkriged <- krigewaterquality(P, grid, 'P','avergvl') 
qwriteRaster(Pkriged, "finaldata/Pkriged.tif", datatype='FLT4S', overwrite=TRUE)

## N
N <-  readOGR("sourcedata", "Nupdated")
N <- spTransform(N, CRS(amersproj))
Nquantile <- as.numeric(quantile(N$avergvl, probs = 0.9))
#normalize
Nscaled <- N$avergvl/Nquantile
Nscaled[which(Nscaled > 1)] <- 1
N$avergvl <- Nscaled 

Nkriged <- krigewaterquality(N, grid, 'N','avergvl') 
writeRaster(Nkriged, "finaldata/Nkriged.tif", datatype='FLT4S', overwrite=TRUE)

