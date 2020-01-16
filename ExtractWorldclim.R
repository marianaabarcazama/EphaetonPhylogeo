# Extract worldclim data for E. phaeton range
# and make maps
# https://rspatial.org/

# Load functions & packages --------------------------------------------------------------------------------------------------- 
source("/Users/marianaabarcazama/Desktop/Projects/MyFunctions.R")
# source("/Users/mar/Desktop/Projects/MyFunctions.R")
library(raster)
library(sf)
library(tidyverse)
library(ggplot2)
library(readxl)
library(cowplot)
library(car)
library(broom)
#library(rasterVis)
library(sf)
library(viridis)

# Import data ------------------------------------------------------------------------------------------------------
#_collection points
datapoints <- read_xlsx("Samples.xlsx", 
                    sheet = "samples", na = c("NA", "")) 
dpoints <- cbind(datapoints$lon, datapoints$lat)

# To add a coordinate reference system
crdref <- CRS('+proj=longlat +datum=WGS84')

# make Spatial object 
points <- SpatialPoints(dpoints, proj4string = crdref)

#_ Get distribution area from shape file
EphaetonRange <- shapefile("euphydryas_phaeton.shp")

#_ Get a polygon of MD
states <- map_data("state")
MDinter <-  subset(states, region %in% c( "maryland"))
MDin <- cbind(MDinter$long, MDinter$lat)
MD <- spPolygons(MDin, crs=crdref)


#_Download worldclim data 
clim = getData('worldclim', var='bio', res=2.5) 
gain(clim) = 0.1

# Crop Eastern USA from each relevant variable 
bio1 <- crop(clim[[1]], extent(-100,-65, 23,50)) # annual mean temperature
bio2 <- crop(clim[[2]], extent(-100,-65, 25,50)) # mean diurnal range
bio3 <- crop(clim[[3]], extent(-100,-65, 25,50)) # isothermality
bio4 <- crop(clim[[4]], extent(-100,-65, 25,50)) # temperature seasonality
bio5 <- crop(clim[[5]], extent(-100,-65, 25,50)) # max temp warmest month
bio6 <- crop(clim[[6]], extent(-100,-65, 25,50)) # min temp coldest month
bio7 <- crop(clim[[7]], extent(-100,-65, 25,50)) # Temperature Annual Range (BIO5-BIO6)
bio10 <- crop(clim[[10]], extent(-100,-65, 25,50)) # Mean Temperature of Warmest Quarter
bio11 <- crop(clim[[11]], extent(-100,-65, 25,50)) # Mean Temperature of Coldest Quarter

# Set a colorblind friendly palette, 
my.palette <- viridis(40,alpha = 0.8, begin = 0, end = 1, option="A")

# Annual mean temperature ----------------------------------------------------------
plot(bio1)
plot(bio1, col = my.palette, main = "Annual mean temperature")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD


# Mean diurnal range ---------------------------------------------------------------------------------
plot(bio2)
plot(bio2, col = my.palette, main = "Mean diurnal range")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD

# Isothermality --------------------------------------------------------------------------------------
plot(bio3)
plot(bio3, col = my.palette, main = "Isothermality")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD

# Temperature seasonality ----------------------------------------------------------------------------
plot(bio4)
plot(bio4, col = my.palette, main = "Temperature seasonality")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD
# Max temp warmest month -----------------------------------------------------------------------------
plot(bio5)
plot(bio5, col = my.palette, main = "Maximum temp warmest month")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD
# min temp coldest month -----------------------------------------------------------------------------
plot(bio6)
plot(bio6, col = my.palette, main = "Min temp coldest month")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD

# Temperature Annual Range -----------------------------------------------------------------------------
plot(bio7)
plot(bio7, col = my.palette, main = "Temperature Annual Range")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD


# Mean temperature of warmest quarter ----------------------------------------------------------------
plot(bio10)
plot(bio10, col = my.palette, main = "Mean temperature of the warmest quarter")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD

# Mean temperature of coldest quarter ----------------------------------------------------------------
plot(bio11)
plot(bio11, col = my.palette, main = "Mean temperature of the coldest quarter")
plot(EphaetonRange, add = TRUE) # add range
plot(points, pch = 16,add = TRUE) # add points (Troubleshoot this part)
plot(MD, add = TRUE) # add MD



