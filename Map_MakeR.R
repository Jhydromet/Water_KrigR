library(tidyverse)
library(lubridate)
library(broom)
library(purrr)
library(sf)
library(rgdal) # Required for raster I think)
library(tmap)  # plot spatially
library(raster)# make raster
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(tmap)
library(raster)


setwd("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Krige Tiffs")



dataday = seq(from = as.Date("2019-04-01"), to = as.Date("2019-09-01"), by = "days")

# Test day dataday = as.Date("2019-04-01")


MappR <-function(dataday){
  
  tif <- raster(paste0("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Krige Tiffs/", dataday, ".tif"))
  
  tm_shape(tif)+ tm_raster()
}






# This code is from here: https://mgimond.github.io/Spatial/interpolation-in-r.html

# read in the DEM for background
DEM <- readAll(raster("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Rasters/WetlandDEMsmall.tif"))

# Water level raster
DAT <- readAll(raster("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Rasters/WaterLevels/contourmaps/17977.tiff"))

crs(DAT) <- crs(DEM)


tm_shape(DAT)+ tm_raster() +
tm_shape(DEM)+ tm_raster(palette = "-Greys") +
  tm_shape(B)+ tm_borders()

