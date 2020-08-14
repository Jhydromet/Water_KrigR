library(tidyverse)
library(lubridate)
library(broom)
library(purrr)
library(sf)
library(plotly)

setwd("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis")

###################### Load up data #######################
files <- dir(pattern = "*-G.csv") # pull the file names from folder
files

# THIS SCRIPT USES THE RAW Capacitance Values, NOT the calculated wl values

data.rw <-  files %>% # load up all files into a longform df
  map(read_csv) %>% 
  reduce(rbind) %>% 
  select(-record, -wl)%>% # remove record, make easy col names
  rename(site = site.no)

data.rw$time <- replace_na(data.rw$time, 00:00) #fix NAs in time (at midnight)
data.rw$datetime = dmy_hms(paste(data.rw$date,data.rw$time))
data.rw$date <- dmy(data.rw$date) # set date format


##### Easy cleaner plots ############

# A1 - Cleaned
# A2 - First day deployment removed. logger was flipped on 26/05/2019. The data prior to that day has been adjusted to top of casing
  # by subtracting 245 from original raw value.
# A3 - Removed first day of deployment and one spike point on 17/08/2019
# A4 - Removed first day, one spike on 21/10/2019
# B1 - Logger flipped on March 31 2020. data largely cleaned
# B2 - Removed first day, site or logger struggles to recover from pumping, removed lagged wl responses to field visits on 26/05/2019, 05/07/2019,10/09/2019 and a spike on 13/11/2019
# B3 - logger is real shitty. Try a rolling avg on this one. Have yet to correct.
# B4 - flawless
# B9 - This well had severe issues with bears tearing the top off. After data download of 31/10/2019, raw_val dropped by 1180 hz. the data following this date has had 1180 added to raw
# C1 - Cleaned
# C2 - Removed first day. REmoved well recoveries on 28/04/2020, 26/05/2020, 10/09/2020,13/11/2020 
# C3 - Died ~ Oct 25 how did I not see this?. had to clean out a bunch of crap in july
# C4 - looks good, though reactive to rain, need to bolster the backfill

data.rw %>%
  ggplot()+
  geom_line(aes(datetime,raw_val, colour = site))


################## Read in spatial, log book, and calibration data ########

shp <- read_sf("C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/Stations_elev.shp") %>% 
  filter(site != "PRECIP" & site != "CRK1" & site != "CRK2" & site != "SLIM" & site != "B5")%>%
  mutate(G = "-G",
         site = paste(site,G, sep = ""))%>%
  select(site,elevations, id)

cal <-read_csv("Calibration Values.csv")

# frsr <- read_sf
  

# Join them to master sheet #


data <- full_join(data.rw, cal, by = "site")
data <- full_join(data, shp)

data <- data %>%
  mutate(cal_tocw_cm = (m*raw_val+b)/10,
         cal_gtw_cm = cal_tocw_cm - toctg_cm,
         cal_wte_m = elevations - (cal_gtw_cm/100)) %>%
  select(-m, -b, -serial) %>% 
  filter(site != "B9-G" & site != "WCRK-G" & site != "ECRK-G")

# check by plot

data %>%
  ggplot()+
  geom_line(aes(datetime,cal_wte_m, colour = site))

# 15 min data

write_csv(data, "15min_waterlevel.csv")

# Change to sf table and summarize to daily averages

daily.wl <- st_as_sf(data) %>% 
  select(date,time,site,cal_wte_m, geometry) %>% 
  group_by(date,site) %>% 
  summarise(cal_wte_m = mean(cal_wte_m, na.rm = T))

# Choose which day you want to interpolate for, put in the filter date, and copy that date to filename below

day <- daily.wl %>% 
  filter(date == "2019-06-18")

write_sf(day, "C:/Users/jmorris/Desktop/Research/Analysis/GIS/Vector Data/WL_shp/wl_2019-06-18.shp")

