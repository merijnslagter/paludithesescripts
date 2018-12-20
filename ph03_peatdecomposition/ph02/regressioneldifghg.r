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
require(RCurl)
library(ggplot2)
library(ggplot2)

ghgtable <- read.csv("sourcedata/heigthghgcatch.csv")
          
p <- ggplot(data = ghgtable, aes(x = heigth, y =averagedrained)) +
            geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
            geom_point() +            
            xlab("Elevation of catchment") +
	    ylab("GHG emissions (CO2-e)") +
	    theme(text = element_text(size=20))
	    
pdf('finaldata/GHGdrained.pdf')   
p
dev.off()	  

p <- ggplot(data = ghgtable, aes(x = heigth, y =averagerewetted)) +
            geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
            geom_point() +            
            xlab("Elevation of catchment") +
	    ylab("GHG emissions (CO2-e)") +
	    theme(text = element_text(size=20))
	    
pdf('finaldata/GHGrewetted.pdf')   
p
dev.off()	

rsq <- function(x, y) summary(lm(y~x))$r.squared
rewettedrsq <- rsq(ghgtable$heigth,ghgtable$averagerewetted)
drainedrsq <- rsq(ghgtable$heigth,ghgtable$averagedrained)
