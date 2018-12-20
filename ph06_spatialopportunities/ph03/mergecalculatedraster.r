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

  peatmin <- raster("sourcedata/peatminraster.tif")
  P <- raster("sourcedata/Pkriged.tif")
  N <- raster("sourcedata/Nkriged.tif")
  peatmin <- resample(peatmin, P)
  writeRaster(peatmin*1, "finaldata/peatminrasterresampled.tif", datatype='FLT4S', overwrite=TRUE)
  
paludisuitscenario <- function(scenario, Nweigth = 2, Pweigth = 1, NPweigth = 1, Peatminindexweigth = 2, Nsuitweigth = 1, Psuitweigth = 1, Decompindexweight = 1, NPsuitweight = 1) {
  ## Map combined N and P map for peat decomposition
  NP <- (N*Nweigth+P*Pweigth)/(Nweigth+Pweigth)
#  writeRaster(NP, "finaldata/NPdecomposition.tif", datatype='FLT4S', overwrite=TRUE)

  ## Map combined peat decomposition vulnerability index and N and P map
  NPpeatmin <- (NP*NPweigth+peatmin*Peatminindexweigth)/(NPweigth+Peatminindexweigth)
#  writeRaster(NPpeatmin, "finaldata/NPpeatmindecomposition.tif", datatype='FLT4S', overwrite=TRUE)

  ## Map suitability of paludiculture regarding N and P
  NPsuit <- (N*Nsuitweigth+P*Psuitweigth)/(Nsuitweigth+Psuitweigth)
#  writeRaster(NPsuit, "finaldata/NPsuitability.tif", datatype='FLT4S', overwrite=TRUE)

  ## Map combining all indices
  Paludisuit <-  (NPsuit*NPsuitweight+NPpeatmin*Decompindexweight)/(Decompindexweight+NPsuitweight)
  scaled <- (rescale(Paludisuit[], to = c(0, 1)))
  Paludisuitcopy <- Paludisuit
  Paludisuitcopy[] <- scaled
  writeRaster(Paludisuitcopy, paste("finaldata/Paludiculturesuitability_", scenario, ".tif", sep =''), datatype='FLT4S', overwrite=TRUE)
  }

paludisuitscenario("Scenario1", Decompindexweight = 1, NPsuitweight = 1)
paludisuitscenario("Scenario2", Decompindexweight = 2, NPsuitweight = 1)
paludisuitscenario("Scenario3", Decompindexweight = 1, NPsuitweight = 2)

difference32 <- raster("finaldata/Paludiculturesuitability_Scenario3.tif") - raster("finaldata/Paludiculturesuitability_Scenario2.tif")
writeRaster(difference32, "finaldata/Paludiculturesuitability_difference32.tif", datatype='FLT4S', overwrite=TRUE)
