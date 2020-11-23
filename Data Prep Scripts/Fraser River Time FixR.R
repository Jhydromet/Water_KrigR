
#fraser time fixer

library(tidyverse)
library(lubridate)

# The fraser data needs to be massaged, from the WSC format (non machine readable), and set to the local datum  on Aug 29 2019. This script finishes standardizing the date time column to have separate date and time cols, to match the water level logger data format.


timeseries <- read_csv("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/Tidied_APPEND_ONLY/FRASER Tidied Date Problems.csv")

ts <- timeseries %>% 
  mutate(datetime = mdy_hm(datetime),
         date = date(datetime),
         time = hms::hms(as.numeric(datetime - floor_date(datetime, "1 day"), unit="secs")),
         trans = "R") %>%
  dplyr::select(record, date, time, raw_val, wl, site.no, trans)
ts

write_csv(ts, "C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis/FRASER-G.csv")
