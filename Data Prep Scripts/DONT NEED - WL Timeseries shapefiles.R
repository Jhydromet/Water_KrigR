
# This script is for turning river line features into a series of points and bonding WL data to those points


library(lubridate)
library(broom)
library(purrr)
library(sf)
library(rgdal) # Required for raster I think)
library(tmap)  # plot spatially
library(raster)# make raster
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(spdplyr)
library(tidyverse)


# read in Fraser river points these points define the channel position

fraser_geo <- read_sf("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/FraserPts.gpkg")

# To make my elevational line feature, I'm using the average start and end point elevations and calculating slope along the line

# start end point (slim creek) at 611 masl, end point (at drainage) at 610.59, over a distance of 2709.08 m

# THIS STEP CAN BE DONE IN QGIS FIELD CALCULATOR

b = 610.59
m = 0.62/2709.08 # rise over run in m/m

# Then add this elevation data to the fraser points

fraser_geo <- fraser_geo %>%
  mutate(elev = (distance*m + b),
         site = distance)

# read in the water level timeseries

fraser_wl <- read_csv("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Rivers/Fraser/fraser2019topresent.csv", col_names = c("date", "wl"), skip = 1) %>% 
  mutate(date = mdy_hm(date))

wl_daily <- fraser_wl %>% 
  mutate(day = date(date)) %>% 
  group_by(day) %>% 
  summarize(wl = mean(wl),
            wl_adjSept4 = wl - 4.864438)

# make a loop, where i is the distance along frsr points, or the index of each fraser_geo entry. The loop will take each individual point and add its appropriate timeseries to that site.

# NEED TO DETERMINE DATE/TIME OF FLIGHT TO PROPERLY SET wl_adj* VALUE FOR OFFSET
# CONSIDER USING MCBRIDE DATA INSTEAD OF HANSARD DUE TO BOWRON INFLUENCE


frsr <- tibble()

for (i in seq(0,2600, by = 50)) {

wl_point <- as_tibble(wl_daily) %>% 
  mutate(distance = i)

point <- as_tibble(fraser_geo) %>% 
  filter(distance == i)

dat <- full_join(point,wl_point) %>% 
  mutate(wt = elev + wl_adjSept4)

frsr <- bind_rows(frsr, dat)
}

# FILTER

frsr %>%
  filter(distance == 100) %>% 
  ggplot()+
  geom_line(aes(day, wt))

frsr_1day <- frsr

frsr_1day <- st_as_sf(frsr_1day) %>% 
  filter(day == "2019-09-04") %>% 
  select(site, wt,geom)

st_crs(frsr_1day) <- st_crs(fraser_geo)

st_write(frsr_1day, "Fraser_20190904.shp")


