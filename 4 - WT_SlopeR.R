library(tidyverse)
library(raster)
library(rgdal)
library(sf)
library(lubridate)


# This script extracts the water table elevation data along a transect of points from interpolated water table rasters, calculates their slopes and saves into a datatable over time.


setwd("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Krige Tiffs")

pts <- read_sf("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Vector Data/GradLinePts.shp")
dataday = seq(from = as.Date("2019-04-01"), to = as.Date("2019-09-01"), by = "days")
# Test day dataday = as.Date("2019-04-01")

gradients <-function(dataday){
  
  gradient <- raster(paste0("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Krige Tiffs/", dataday, ".tif"))
  
  pts$wt <- raster::extract(x = gradient, y = pts)
  
  ptsdat <- as_tibble(pts) %>% 
    summarise(date = dataday,
              slope = abs((last(wt) - first(wt))/(last(distance)-first(distance))), #m/m rise/run
              darcyq = slope*(2.798596e-05)*31557600) # value for conductivity from Theis script in cm/s. units here cm/y 
}

slopes <- lapply(dataday, gradients) %>% bind_rows()


slopes %>% 
  ggplot()+
  geom_line(aes(x = date, y = slope))+
  labs(title = "Water Table Gradient", x = "Date", y = "dh/dx")

slopes %>% 
  ggplot()+
  geom_line(aes(x = date, y = darcyq))+
  labs(title = "Darcy Flux", x = "Date", y = "darcy flux (cm/y)")

# REMEMBER THIS IS NOT VELOCITY. NEED TO DIVIDE BY POROSITY FOR AVG LINEAR VELOCITY.

# Porosity in sandy silt can range from 25%-50%, so avg linear velo could be ~60-30 cm/yr
