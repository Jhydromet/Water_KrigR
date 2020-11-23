
library(tidyverse)
library(lubridate)
library(broom)
library(purrr)
library(sf)
library(rgdal) # Required for raster I think
library(raster)# make raster
library(sp)    # Used for the spsample function
library(plotly)

# Slim Creek --------------------------------------------------------------

# River Timeseries BindR --------------------------------------------------
setwd("C:/Users/jmorris/Desktop/Research/Analysis/GIS")

rivergeo <- sf::read_sf("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/SlimPts_elev.shp")

timeseries <- read_csv("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis/SLIM.csv") %>% 
  mutate(site = "SLIM") %>% 
  dplyr::select(date,time,raw_val,site) %>% 
  list()


# Add site ID to timeseries -----------------------------------------------
distance <- rivergeo$distance

rivertime <- tibble(distance) %>% 
  mutate(timeseries) %>%
  tidyr::unnest(cols = timeseries)


# Bind the timeseries to shapefile, calculate water levels ----------------
meta <- read_csv("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis/Calibration Values.csv")

SlimData <- full_join(rivertime, meta, by = "site")
SlimData <- full_join(SlimData, rivergeo)

SlimData <- SlimData %>%
  mutate(wl = (m*raw_val+b)/10,
         cal_wte_m = elevations - (wl/100)) %>%
  dplyr::select(-m, -b,)


# -------------------------------------------------------------------------
# Fraser River ------------------------------------------------------------

# River Timeseries BindR --------------------------------------------------
setwd("C:/Users/jmorris/Desktop/Research/Analysis/GIS")

rivergeo <- sf::read_sf("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/FraserPts_elev.shp") %>% 
  dplyr::select(-id,-Length,-angle)


rivergeo <- cbind(rivergeo, st_coordinates(rivergeo))

timeseries <- read_csv("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis/FRASER.csv") %>%
  mutate(site = site.no,
         datetime = mdy_hm(datetime),
         date = date(datetime)) %>%
  select(-record, -time) %>% 
  list()



# Add site ID to timeseries -----------------------------------------------
distance <- rivergeo$distance

rivertime <- tibble(distance) %>% 
  mutate(timeseries) %>%
  tidyr::unnest(cols = timeseries)


FraserData <- full_join(rivertime, rivergeo)


FraserData <- FraserData %>%
  mutate(cal_wte_m = elevations + wl)%>% 
  select(site,date,datetime,trans,cal_wte_m,X,Y)


daily.FraserData <- FraserData%>% 
  dplyr::group_by(site, date, trans, X, Y) %>% 
  dplyr::summarise(cal_wte_m = mean(cal_wte_m)) %>% 
  distinct()

daily.FraserData <- st_as_sf(daily.FraserData, coords = c("X", "Y"), crs = 32610)




st_sfc(fuck)


# test <- FraserData %>% 
#   dplyr::filter(distance == 1300) %>%
#   dplyr::select(datetime, cal_wte_m)
# 
# plt <- test %>% 
#   ggplot()+
#   geom_line(aes(mdy_hm(datetime),cal_wte_m))
# 
# ggplotly(plt)



# Write the new point datafile --------------------------------------------


st_write(daily.FraserData, "C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/Fraser_Timeseries.shp")
