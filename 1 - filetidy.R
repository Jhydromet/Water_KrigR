library(tidyverse)
library(lubridate)
library(broom)
library(purrr)

setwd("C:/Users/jmorris/Desktop/Research/Analysis/Water Levels/Working Data/Groundwater/For Analysis")

files <- dir(pattern = "*.csv")
files

dtmn.fn<-function(x){
        x
        nmf<-substr(x,1,4)
        nmf
        trans <- substr(x,1,1)
        trans
        x.df<-read_csv(x,skip=9, col_names = F)
        x.df
        colnames(x.df)<-c("record","date","time","raw_val","wl")
        x.df$site.no<-nmf
        x.df$trans<-trans
        filenm <- paste(nmf, ".csv", sep = "")
        filenm
        write_csv(x.df,filenm)
        return(x.df)
}

lapply("C1-G.csv",dtmn.fn)

 