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


## Make grid that is used to visualize suitability of paludiculture in the Netherlands

amersproj <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
buffered <- readOGR("sourcedata","peatbuffered1000")
buffered <- spTransform(buffered, amersproj)
min_x = extent(buffered)[1] #minimun x coordinate
min_y = extent(buffered)[3] #minimun y coordinate
x_length = max(extent(buffered)[2] - min_x) #easting amplitude
y_length = max(extent(buffered)[4] - min_y) #northing amplitude
cellsize = 250 #pixel size
ncol = round(x_length/cellsize,0) #number of columns in grid
nrow = round(y_length/cellsize,0) #number of rows in grid
grid = GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(cellsize,cellsize),cells.dim=c(ncol,nrow))
#Convert GridTopolgy object to SpatialPixelsDataFrame object.
grid = SpatialPixelsDataFrame(grid,
                              data=data.frame(id=1:prod(ncol,nrow)),
                              proj4string=CRS(amersproj))
shp = buffered@polygons
shp = SpatialPolygons(shp, proj4string=CRS(amersproj)) #make sure the shapefile has the same CRS from the data, and from the prediction grid.
grid = grid[!is.na(over(grid, shp)),]
a <- grid
grid.df<- data.frame(grid@coords)
names(grid.df) <- c("x","y")
coordinates(grid.df) <-  ~ x + y

rastergrid <- rasterFromXYZ(grid.df)
rastergrid <- 0

setwd("finaldata")
save(grid, file ="pixelgrid.rda")