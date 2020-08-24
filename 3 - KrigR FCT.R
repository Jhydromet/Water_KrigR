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

setwd("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data")

basemap <- raster::raster("C:\\Users\\jmorris\\Desktop\\Research\\Analysis\\GIS\\Rasters\\WetlandDEMsmall.tif")

files <- dir(pattern = "*.shp")
files

dataday = "2019-07-01"

KrigR <- function(dataday) {
  
  WL <- as_Spatial(st_read(paste0("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/WL_shp/", ymd(dataday), ".shp")))
  B <-  as_Spatial(st_read("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/AOIreproj.gpkg"))
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
  
  writeRaster(r.m, filename= paste0("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Rasters/WL_KrigeMaps/", dataday, ".tif"), format="GTiff", overwrite=TRUE)
  
  WL$wt_elev <- sprintf("%0.1f", WL$cal_wte_m)

}


lapply(files, KrigR)





# 
# plt <- tm_shape(basemap)+
#   tm_raster(palette = "Greys")+
#   tm_shape(r.m) +
#   tm_raster(n=10, palette="Blues", auto.palette.mapping=FALSE, 
#             title="Water Table Elevation \n(m.a.s.l)", midpoint = NA) +
#   tm_shape(WL) + tm_dots(size=0.2) +
#   tm_text('wt_elev', just="left", xmod=.5, size = 0.7) +
#   tm_legend(legend.outside=TRUE)




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
