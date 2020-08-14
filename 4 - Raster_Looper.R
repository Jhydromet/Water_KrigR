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


##########################################################################

# This code is from here: https://mgimond.github.io/Spatial/interpolation-in-r.html

# read in wetland boundary
B <-  as_Spatial(st_read("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/AOIreproj.gpkg"))

# read in water level 
WL <- as_Spatial(st_read("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis/waterlevel.gpkg"))

# Set bounding box of water level file to that of boundary
WL@bbox <- B@bbox

WL$X <- coordinates(WL)[,1]
WL$Y <- coordinates(WL)[,2]


# select main columns, create daily avg and add X and Y cols
dailyWL <- st_as_sf(WL) %>% 
  select(site, datetime, X, Y, cal_wte_m) %>% 
  mutate(date = date(datetime)) %>% 
  group_by(site, date, X, Y) %>% 
  summarise(wl = mean(cal_wte_m))

# Make arbitrary grid to recieve kriged surface
grd         <- as.data.frame(spsample(WL, "regular", n = 50000))
names(grd)  <- c("X","Y")
coordinates(grd) <- c("X","Y")
gridded(grd)  <- TRUE
fullgrid(grd) <- TRUE
proj4string(grd) <- proj4string(WL)

# Set detrending formula 1st order polynomial
f.1 <- as.formula(wl ~ X + Y)



for (i in seq(as.Date('2019-03-22'),as.Date('2020-04-07'),by = 1)){
  filename <- paste("WL",i, sep = "_")
  data <- as_Spatial(filter(dailyWL, date == i))
  var.smpl <- variogram(f.1, data, cloud = FALSE, cutoff=1000000, width=89900)
  dat.fit <-  fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill = 13, model = "Sph", range = 590000, nugget = 0))
  dat.krg <- krige(f.1, data, grd, dat.fit)

  r <- raster(dat.krg)
  r.m <- mask(r, B)
  writeRaster(r.m, filename= paste0("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Rasters/WaterLevels/contourmaps", filename, ".tiff"), format="GTiff", overwrite=TRUE)
}






