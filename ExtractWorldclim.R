# Extract worldclim data for E. phaeton range

# Load functions & packages --------------------------------------------------------------------------------------------------- 
source("/Users/marianaabarcazama/Desktop/Projects/MyFunctions.R")
# source("/Users/mar/Desktop/Projects/MyFunctions.R")
library(raster)
library(sf)

# Worldclim files are large and not included in this repo
# go to "https://www.worldclim.org/" and select version 2,
# 2.5 minutes resolution to download.

# Code adapted from:
# https://www.benjaminbell.co.uk/2018/02/rasterstacks-and-rasterplot.html

# get a list of the names of all files for each variable (prec, tmin, etc)
precFiles <- list.files("/Users/marianaabarcazama/DocumentsII/Worldclim2_2.5/wc2.0_2.5m_prec/", ".tif", full.names = TRUE)
tminFiles <- list.files("/Users/marianaabarcazama/DocumentsII/Worldclim2_2.5/wc2.0_2.5m_tmin/", ".tif", full.names = TRUE)
tmaxFiles <- list.files("/Users/marianaabarcazama/DocumentsII/Worldclim2_2.5/wc2.0_2.5m_tmax/", ".tif", full.names = TRUE)
tavgFiles <- list.files("/Users/marianaabarcazama/DocumentsII/Worldclim2_2.5/wc2.0_2.5m_tavg/", ".tif", full.names = TRUE)
bioFiles <- list.files("/Users/marianaabarcazama/DocumentsII/Worldclim2_2.5/wc2.0_2.5m_bio/", ".tif", full.names = TRUE)

# combine files into one object (class: RasterStack)
prec <- stack(precFiles) 
tmin <- stack(tminFiles)
tmax <- stack(tmaxFiles)
tavg <- stack(tavgFiles)
bio <- stack(bioFiles)

# Rename layers in the RasterStack
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
biovars <- c("BIO1", "BIO2", "BIO3", "BIO4", "BIO5", "BIO6", "BIO7", "BIO8", "BIO9", "BIO10",
             "BIO11", "BIO12", "BIO13", "BIO14", "BIO15", "BIO16", "BIO17", "BIO18", "BIO19")
names(prec) <- month
names(tmin) <- month
names(tmax) <- month
names(tavg) <- month
names(bio) <- biovars

#Morocco <- getData('GADM', country="USA", level=0)
plot(Morocco)

library(tidyverse)
library(ggplot2)
library(readxl)
library(cowplot)
# library(MASS)
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

ggplot(data = states)+
  geom_polygon(aes(x = long, y = lat, group = group), color = "darkgrey", fill = "lightgrey")+
  #geom_polygon(data = Sampled, mapping = aes(x = long, y = lat, group = group), color = "darkgrey", fill = "darkgrey")+
  geom_point(data = points, mapping = aes(x = lon, y = lat, shape = factor(host)) )+
  scale_shape_manual(values = c(1,1,16,1))+
  geom_sf(data = drange, size = 0.2, color = "black", fill = "#E69F00", alpha = 0.3) +
  theme_cowplot()+
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
clim=getData('worldclim', var='bio', res=10) 
gain(clim) = 0.1
ggplot(clim[[1:3]])+geom_raster(aes(fill=value))+
  facet_wrap(~variable)+
  scale_fill_gradientn(colours=c("brown","red","yellow","darkgreen","green"),trans="log10")+
  coord_equal()
plot(clim[[1:3]])
bio1 <- crop(clim[[1]], extent(-100,-65, 25,50)) # annual mean temperature
bio2 <- crop(clim[[2]], extent(-100,-65, 25,50)) # mean diurnal range
bio3 <- crop(clim[[3]], extent(-100,-65, 25,50)) # isothermality
bio4 <- crop(clim[[4]], extent(-100,-65, 25,50)) # temperature seasonality
bio5 <- crop(clim[[5]], extent(-100,-65, 25,50)) # max temp warmest month
bio6 <- crop(clim[[6]], extent(-100,-65, 25,50)) # min temp coldest month
bio10 <- crop(clim[[7]], extent(-100,-65, 25,50)) # Mean Temperature of Warmest Quarter
bio11 <- crop(clim[[8]], extent(-100,-65, 25,50)) # Mean Temperature of Coldest Quarter

plot(bio1)
plot(bio2)
# Temperature maps ------------------------------------------------------------------------------

# Import temperature data (downloaded from WorldClim2)
# Tutorial:
# https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html
# https://cmerow.github.io/RDataScience/05_Raster.html#5_worldclim
library(raster)
library(rgdal)
temp1 <- raster("wc2.0_2.5m_tmin_01.tif")

tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
#Next, we'll plot a map of global January temperatures using this colour scheme:

plot(temp1, col=tempcol(100))
plot(temp1, xlim=c(-130, -65), ylim=c(25, 50), col=tempcol(100))
# Next step: combine temperature and range maps, plot also min, max, avg temperature and precipitation
