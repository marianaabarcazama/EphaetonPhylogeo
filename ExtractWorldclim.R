# Extract worldclim data for E. phaeton range

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
library(rasterVis)

# Import data (leps only)------------------------------------------------------------------------------------------------------
points <- read_xlsx("Samples.xlsx", 
                    sheet = "samples", na = c("NA", "")) 


usa <- map_data("usa")
states <- map_data("state")   # to load data to make usa maps with individual states 
Sampled <- subset(states, region %in% c( "vermont",  "massachusetts", "michigan",
                                         "arkansas", "pennsylvania", "new hampshire", 
                                         "new york"))
drange <- st_read("euphydryas_phaeton.shp") # read shape file

a <- ggplot(data = states)+
  geom_polygon(aes(x = long, y = lat, group = group), color = "darkgrey", fill = "lightgrey")+
  #geom_polygon(data = Sampled, mapping = aes(x = long, y = lat, group = group), color = "darkgrey", fill = "darkgrey")+
  geom_point(data = points, mapping = aes(x = lon, y = lat, shape = factor(host)) )+
  scale_shape_manual(values = c(1,1,16,1))+
  geom_sf(data = drange, size = 0.2, color = "black", fill = "#E69F00", alpha = 0.3) +
  #theme_cowplot()+
  ylab("Latitude")+
  xlab("Longitude")+
  #theme(legend.position = "none")+
  # modify this line to add state names automatically
  #geom_text(data= names, aes(long, lat, label = state), size=2)
  # annotate("text", x = -93, y = 34, label = "AK")+
  # annotate("text", x = -85, y = 45, label = "MI")+
  # annotate("text", x = -80, y = 41, label = "NY")+
  # annotate("text", x = -71, y = 41, label = "VT")+
  #coord_fixed(1.3, xlim = c(-94, -65))
  coord_sf() # this is necesary for geom_sf to work.
# Download worldclim data
clim = getData('worldclim', var='bio', res=2.5) 
gain(clim) = 0.1
gplot(clim[[1:3]])+geom_raster(aes(fill=value))+
  facet_wrap(~variable)+
  scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"),trans="log10")+
  coord_equal(xlim = c(-94, -65), ylim = c(23, 50))
plot(clim[[1:3]])
bio1 <- crop(clim[[1]], extent(-100,-65, 23,50)) # annual mean temperature
bio2 <- crop(clim[[2]], extent(-100,-65, 25,50)) # mean diurnal range
bio3 <- crop(clim[[3]], extent(-100,-65, 25,50)) # isothermality
bio4 <- crop(clim[[4]], extent(-100,-65, 25,50)) # temperature seasonality
bio5 <- crop(clim[[5]], extent(-100,-65, 25,50)) # max temp warmest month
bio6 <- crop(clim[[6]], extent(-100,-65, 25,50)) # min temp coldest month
bio10 <- crop(clim[[7]], extent(-100,-65, 25,50)) # Mean Temperature of Warmest Quarter
bio11 <- crop(clim[[8]], extent(-100,-65, 25,50)) # Mean Temperature of Coldest Quarter

plot(bio1)
plot(bio2)
plot(bio3)
plot(bio4)
plot(bio5)
plot(bio6)
plot(bio10)
plot(bio11)

b <- gplot(bio11, maxpixels = 50000000)+geom_raster(aes(fill=value))+
  facet_wrap(~variable)+
  scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"),trans="log10")+
  #geom_point(data = points, mapping = aes(x = lon, y = lat, shape = factor(host)) )+
  #geom_sf(data = drange, size = 0.2, color = "black", fill = "#E69F00", alpha = 0.3) +
  coord_equal(xlim = c(-94, -65), ylim = c(23, 50))

ggplot(data = states)+
  geom_polygon(aes(x = long, y = lat, group = group), color = "darkgrey", fill = "lightgrey")+
  #geom_polygon(data = Sampled, mapping = aes(x = long, y = lat, group = group), color = "darkgrey", fill = "darkgrey")+
  geom_point(data = points, mapping = aes(x = lon, y = lat, shape = factor(host)) )+
  scale_shape_manual(values = c(1,1,16,1))+
  geom_sf(data = drange, size = 0.2, color = "black", fill = "#E69F00", alpha = 0.3) +
  geom_raster(data = bio11, mapping =aes(fill = value))+
  #theme_cowplot()+
  ylab("Latitude")+
  xlab("Longitude")+
  #theme(legend.position = "none")+
  # modify this line to add state names automatically
  #geom_text(data= names, aes(long, lat, label = state), size=2)
  # annotate("text", x = -93, y = 34, label = "AK")+
  # annotate("text", x = -85, y = 45, label = "MI")+
  # annotate("text", x = -80, y = 41, label = "NY")+
  # annotate("text", x = -71, y = 41, label = "VT")+
  #coord_fixed(1.3, xlim = c(-94, -65))
  coord_sf() # this is necesary for geom_sf to work.

# Next step: combine temperature and range maps, plot also min, max, avg temperature and precipitation
library(rasterVis)
library(sp)

# Download States boundaries (might take time)
out <- getData('GADM', country='United States', level=1)

# Extract California state
California <- out[out$NAME_1 %in% 'California',]

# Plot raster and California:

levelplot(RAD2012.all) + 
  layer(sp.polygons(California))
plot_grid(a,b)
