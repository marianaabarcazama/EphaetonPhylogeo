
# Map collection points

# Load functions & packages --------------------------------------------------------------------------------------------------- 
source("/Users/marianaabarcazama/Desktop/Projects/MyFunctions.R")
 source("/Users/mar/Desktop/Projects/MyFunctions.R")

library(tidyverse)
library(readxl)
library(cowplot)
# library(MASS)
library(car)
library(broom)

# Import data (leps only)------------------------------------------------------------------------------------------------------
points <- read_xlsx("Samples.xlsx", 
                    sheet = "samples", na = c("NA", "")) 


map_data("state") %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "grey", col = "grey", alpha = 0.5)+
  geom_point(data = points, mapping = aes(x = lon, y = lat, group = NA), shape = 1)+
  #geom_point(data = wholedata, mapping = aes(x = lon, y = lat, group = NA, col = sp, label = sp))+
  #geom_text(data = wholedata, mapping = aes(x = lon, y = lat, label = sp, group = NA))+
  theme_cowplot()#+
 
### All sites States
usa<-map_data("usa")
states<-map_data("state")   # to load data to make usa maps with individual states 
Sampled<-subset(states, region %in% c( "vermont",  "massachusetts", "michigan",
                                       "arkansas", "pennsylvania", "new hampshire", 
                                       "new york"))



ggplot(data = states)+
  geom_polygon(aes(x = long, y = lat, group = group), color = "darkgrey", fill = "lightgrey")+
  geom_polygon(data = Sampled, mapping = aes(x = long, y = lat, group = group), color = "darkgrey", fill = "darkgrey")+
  geom_point(data = points, mapping = aes(x = lon, y = lat) )+
  # modify this line to add state names automatically
  #geom_text(data= names, aes(long, lat, label = state), size=2)
  annotate("text", x = -93, y = 34, label = "AK")+
  annotate("text", x = -85, y = 45, label = "MI")+
  annotate("text", x = -80, y = 41, label = "NY")+
  annotate("text", x = -71, y = 41, label = "VT")+
  #annotate("text", x = -80, y = 41, label = "NY")+
  coord_fixed(1.3, xlim = c(-94, -65)) # so you can change the size of the map, but the proportion y 1.3 times larger

