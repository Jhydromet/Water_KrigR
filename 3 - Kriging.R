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


# The kriging code is from here: https://mgimond.github.io/Spatial/interpolation-in-r.html



setwd("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data")

# read in wetland boundary and water level shape files
B <-  as_Spatial(st_read("AOIreproj.gpkg"))
WL <- as_Spatial(st_read("WL_shp/wl_2019-06-18.shp"))

# Set bounding box of water level file to that of boundary
WL@bbox <- B@bbox

testdat <- st_as_sf(WL)

# Plot to check

tm_shape(B) + tm_polygons() +
  tm_shape(WL) +
  tm_dots(col="cal_wte_m", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Water table elevation \n(in m)", size=0.7) +
  tm_text("cal_wte_m", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)


# Make an arbitrary grid to recieve the kriged surface

grd         <- as.data.frame(spsample(WL, "regular", n = 50000))
names(grd)  <- c("X","Y")
coordinates(grd) <- c("X","Y")
gridded(grd)  <- TRUE
fullgrid(grd) <- TRUE
proj4string(grd) <- proj4string(WL)    


# Define first order polynomial equation (This is for detrending in the variogram step)
# !!!!!! I will likely need to determine what an appropriate detrending approach to use is!!!!

f.1 <- as.formula(cal_wte_m ~ X + Y)

# Add X and Y to WL
WL$X <- coordinates(WL)[,1]
WL$Y <- coordinates(WL)[,2]


# Compute the sample variograml note that the f.1 trend model is one of the parameters
# passed to variogram(). This tells the funciton to create the variogram on the de-trended data.

# I don't know how the cutoff and width values are chosen but they do effect the output

var.smpl <- variogram(f.1, WL, cloud = FALSE, cutoff=1000000, width=89900)


# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.

dat.fit <-  fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill = 14, model = "Sph", range = 590000, nugget = 0))

plot(var.smpl, dat.fit)
plot

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(formula = f.1, locations = WL, newdata = grd, model = dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, B)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Water Table Elevation \n(m.a.s.l)") +
  tm_shape(WL) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

writeRaster(r.m, filename="C:/Users/jmorris/Desktop/Research/Analysis/GIS/Rasters/WL_KrigeMaps/wl_2019-06-18krige.tif", format="GTiff", overwrite=TRUE)

# Plot variance data. This is generated when Krige() is used

r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, B)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \n(m)") +tm_shape(WL) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# Plot the 95% confidence interval. Interpret this as the m above or below estimated).

r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, B)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map \n(m)") +tm_shape(WL) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
