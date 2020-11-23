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
library(mapview)

setwd("E:/Jeremy's MSc Research/Hydrometric and GIS/Water Data/Daily_WL_Shapefiles")

dataday = seq(from = as.Date("2019-04-01"), to = as.Date("2019-09-01"), by = "days")
# Test day dataday = as.Date("2019-04-01")

KrigR <- function(dataday) {
  
  WL <- as_Spatial(st_read(paste0(ymd(dataday), ".shp")))
  B <-  as_Spatial(st_read("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Vector Data/AOI.gpkg"))
  WL@bbox <- B@bbox
  grd         <- as.data.frame(spsample(WL, "regular", n = 50000))
  names(grd)  <- c("X","Y")
  coordinates(grd) <- c("X","Y")
  gridded(grd)  <- TRUE
  fullgrid(grd) <- TRUE
  proj4string(grd) <- proj4string(WL)    
  f.1 <- as.formula(cal_wte_m ~ X + Y)
  WL$X <- coordinates(WL)[,1]
  WL$Y <- coordinates(WL)[,2]
  var.smpl <- variogram(f.1, WL, cloud = FALSE, cutoff=1000000, width=89900)
  dat.fit <-  fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                            vgm(psill = 14, model = "Sph", range = 590000, nugget = 0))
  dat.krg <- krige(formula = f.1, locations = WL, newdata = grd, model = dat.fit)
  r <- raster(dat.krg)
  r.m <- mask(r, B)
  
  writeRaster(r.m, filename= paste0("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Krige Tiffs/", dataday, ".tif"), format="GTiff", overwrite=TRUE)
  
  WL$wt_elev <- sprintf("%0.1f", WL$cal_wte_m)

}


lapply(dataday, KrigR)

mapview(B)


basemap <- raster::raster("E:/Jeremy's MSc Research/Hydrometric and GIS/GIS Data/Raster Data/Ancient Forest Wetland LIDAR/AF_DEM.tif")

plt <-   tm_shape(r.m) +
  tm_raster(n=10, palette="Blues", auto.palette.mapping=FALSE,
            title="Water Table Elevation \n(m.a.s.l)", midpoint = NA) +
  tm_shape(WL) + tm_markers(text = "cal_wte_m",size=0.2) +
  tm_text("cal_wte_m", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

plt


# Variance plotting, could be added later to loop? maybe doesnt matter -------------------------



# Plot variance data. This is generated when Krige() is used
# 
# r   <- raster(dat.krg, layer="var1.var")
# r.m <- mask(r, B)
# 
# tm_shape(r.m) + 
#   tm_raster(n=7, palette ="Reds",
#             title="Variance map \n(m)") +tm_shape(WL) + tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE)
# 
# # Plot the 95% confidence interval. Interpret this as the m above or below estimated).
# 
# r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
# r.m <- mask(r, B)
# 
# tm_shape(r.m) + 
#   tm_raster(n=7, palette ="Reds",
#             title="95% CI map \n(m)") +tm_shape(WL) + tm_dots(size=0.2) +
#   tm_legend(legend.outside=TRUE)
