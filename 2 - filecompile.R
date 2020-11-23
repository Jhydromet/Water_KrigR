library(tidyverse)
library(lubridate)
library(broom)
library(purrr)
library(sf)
library(sp)
library(plotly)
library(mapview)

setwd("E:/Jeremy's MSc Research/Hydrometric and GIS/Water Data/Groundwater/For Analysis")

###################### Load up data #######################
files <- dir(pattern = "*-G.csv") # pull the file names from folder
files

# THIS SCRIPT USES THE RAW Capacitance Values, NOT the calculated wl values

data.rw <-  files %>% # load up all files into a longform df
  map(read_csv) %>% 
  reduce(rbind) %>% 
  dplyr::select(-record, -wl)%>% # remove record, make easy col names
  rename(site = site.no)

data.rw$time <- replace_na(data.rw$time, 00:00) #fix NAs in time (at midnight)
data.rw$datetime = dmy_hms(paste(data.rw$date,data.rw$time))
data.rw$date <- dmy(data.rw$date) # set date format

# current data plot

p <- data.rw %>%
  ggplot()+
  geom_line(aes(datetime,site, colour = site), size = 2) + 
  labs(x = "Date", y = "Site", title = paste0(Sys.Date()," Tidied WL Logger Data"))+
  scale_colour_discrete(guide = FALSE)

p

ggsave(paste0("E:/Jeremy's MSc Research/Hydrometric and GIS/Water Data/Groundwater/Plots/",Sys.Date(),"_Tidied_WL_Data.png"), p, scale = .95)

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




# -------------------------------------------------------------------------
# Read in spatial, log book, and calibration data, join them --------------

# piezo sites
shp <- read_sf("Piezo_elev.shp") %>% 
  filter(site != "PRECIP" & site != "CRK1" & site != "CRK2" & site != "SLIM" & site != "B5")%>%
  mutate(G = "-G",
         site = paste(site,G, sep = ""))%>%
  dplyr::select(site,elevations, id)

# plot to check
mapview(shp)
  

cal <-read_csv("Calibration Values.csv")

data <- full_join(data.rw, cal, by = "site")
data <- full_join(data, shp)
data <- st_as_sf(data)

# Calculate calibrated water levels (top of casing to water, ground to water, water elevation) Then filter out the unwanted columns

data <- data %>%
  mutate(cal_tocw_cm = (m*raw_val+b)/10,
         cal_gtw_cm = cal_tocw_cm - toctg_cm,
         cal_wte_m = elevations - (cal_gtw_cm/100)) %>%
  dplyr::select(-m, -b, -serial) %>% 
  filter(site != "B9-G" & site != "WCRK-G" & site != "ECRK-G" & site != "SLIM")

# group by day and site, then take daily means

daily.data <- data %>% 
  dplyr::group_by(site, date, trans) %>% 
  dplyr::summarise(cal_wte_m = mean(cal_wte_m)) %>% 
  ungroup()


###############################################################################################
# Bind in Fraser dataset too

fraser <- read_sf("Fraser_Timeseries.shp") %>% 
  mutate(site = as.character(site),
         trans = as.character(trans))


# pltt to check sites
mapview(fraser)

bonded <- rbind(daily.data, fraser)

# check by plot

p <- bonded %>%
  filter(date >= dmy("01-04-2019") & date <= dmy('01-09-2019')) %>% 
  group_by(site, date, trans) %>% 
  summarise(cal_wte_m = mean(cal_wte_m)) %>% 
  ggplot()+
  geom_line(aes(date,cal_wte_m, colour = site, linetype = trans))+
  labs(x = "Month", y = "Water Table Elevation (m.a.s.l.)", title = "Water Level Data 2019", colour = "Site", linetype = "Transect")

p

ggsave("C:/Users/jmorris/Desktop/Research/Diagrams/Aug2020/2019_WT_TimeseriesPlot.png", p, dpi = 300, scale = 0.95)


# Write up the master 15 min datasheet

write_csv(bonded, "daily_waterlevel.csv")



# -------------------------------------------------------------------------
# Define Date Range of Interest, Create Daily Mean WL Shapefiles ---------

daterange <- seq(ymd("2019-04-01"), ymd("2019-09-01"), by = "day")

dataday = "2019-04-01"

WL_SHAPER <- function(daterange){
  
daily.wl <- st_as_sf(bonded) %>% 
  dplyr::select(date,site,cal_wte_m, geometry) %>% 
  filter(date == daterange)

st_write(daily.wl, paste0("E:/Jeremy's MSc Research/Hydrometric and GIS/Water Data/Daily_WL_Shapefiles/", daterange, ".shp"))
}

lapply(daterange,WL_SHAPER)
