
# This Script will use the lat/long of point data to find their elevations in the lidar DEM

library(raster)
library(rgdal)
library(sf)
library(mapview)

setwd("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data")


dem <- raster("Raster Data/Ancient Forest Wetland LIDAR/AF_DEM.tif")
well_sites <- read_sf("E:/Jeremy's MSc Research/Hydrometric and GIS/Water Data/Groundwater/For Analysis/Piezos.shp")

well_sites$elevations <- extract(x = dem, y = well_sites)

mapview(well_sites)


st_write(well_sites,"Vector Data/Piezo_elev.shp")


