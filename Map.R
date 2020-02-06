
# Map collection points

# Load functions & packages --------------------------------------------------------------------------------------------------- 
source("/Users/marianaabarcazama/Desktop/Projects/MyFunctions.R")
# source("/Users/mar/Desktop/Projects/MyFunctions.R")

library(tidyverse)
library(readxl)
library(cowplot)
# library(MASS)
library(car)
library(broom)
library(sf)


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

