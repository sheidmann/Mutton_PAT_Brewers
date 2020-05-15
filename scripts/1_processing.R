# 1_processing.R

# Sarah Heidmann
# Created 8 Oct 2018
# Last modified 15 May 2020

# Summary:
# Data inputs:
#     - mutton snapper acoustic data: 1_processed_JKM (raw data)
# Actions:
#     - removes detections at other arrays
#     - splits the date into multiple columns
#     - classifies detections as day, night, or crepuscular
#     - projects positions from lat/long to UTM_20N 
# Data exports:
#     - 2_processed_SLH (formatted raw data)

# Load libraries
library(tidyverse)
library(lubridate)
library(sp)

##### Import data #####
# Uses processed data (1_processed_JKM)
sourcePath <- "data/1_processed_JKM/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
importMNB <- function(filename){
   # Read the file
   dat <- read_csv(paste0(sourcePath, filename),
                   col_types = cols(station = col_character())) %>%
      # Keep only needed columns
      select(station,transmitter, detection_time_ast, lat_nad83,long_nad83) %>%
      # Change time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, 
                                           "America/Virgin")) %>%
      # Sort by time
      arrange(detection_time_ast)
   # Return the dataset
   return(dat)
}
mnbls <- lapply(filenames, importMNB)
names(mnbls) <- substr(filenames, 1, 14)
mnbls

sinkPath <- "data/2_processed_SLH/"

##### Delete spawning detections #####
# Load list of active Brewers receivers
statmaster <- read_csv("data/otherdata/mnb_station_master_2017.csv")
# Keep only detections on that list
delStat <- function(dataset){
      dat <- filter(dataset, station %in% statmaster$station)
      return(dat)
}
mnbls_proc <- lapply(mnbls, delStat)

##### Split date #####
splitDate <- function(dataset){
      dataset <- dataset %>%
         mutate(date = date(detection_time_ast), 
                hour = hour(detection_time_ast), 
                min = minute(detection_time_ast),
                sec = second(detection_time_ast))
      print("Date split complete")
      return(dataset)
}
mnbls_proc <- lapply(mnbls_proc, splitDate)

##### Calculate time differences #####
calcTimeDiff <- function(dataset){
   # Subtract time
      dataset <- dataset %>%
         mutate(timediff= c(as.numeric(tail(detection_time_ast, -1)) -
                                  as.numeric(head(detection_time_ast, -1)), NA))
      print("Time difference calculation complete")
      return(dataset)
}
mnbls_proc <- lapply(mnbls_proc, calcTimeDiff)

##### Classify day/night #####
suntab <- read_csv("data/otherdata/sttsuntable_14_18.csv", 
                   col_types = cols(rise = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
                                    set = col_datetime(format = "%Y/%m/%d %H:%M:%S")))
addDayNight <- function(dataset){
   dataset <- left_join(dataset, suntab, by="date")
   dataset <- dataset %>%
      mutate(daynight=case_when(detection_time_ast<rise-3600 | detection_time_ast>set+3600~"night",
                                detection_time_ast>rise+3600 & detection_time_ast<set-3600~"day",
                                (detection_time_ast>=rise-3600 & detection_time_ast<=rise+3600) |
                                   (detection_time_ast>=set-3600 & detection_time_ast<=set+3600)~"crep"))
   print("Day/night classification complete")
   return(dataset)
}
mnbls_proc <- lapply(mnbls_proc, addDayNight)

# add UTM20 positions (x_UTM20 and y_UTM20) so don’t have to project every time
projUTM20 <- function(dataset){
      dataset_s <- SpatialPoints(coords = select(dataset, long_nad83, lat_nad83),
                                 proj4string = CRS("+proj=longlat +datum=NAD83"))
      #project (UTM Zone 20N)
      dataset_sp <- spTransform(dataset_s, CRS("+proj=utm +zone=20 +datum=NAD83 +units=m"))
      coords <- as_tibble(dataset_sp) %>%
         rename(x_UTM20N=long_nad83, y_UTM20N=lat_nad83)
      dataset <- bind_cols(dataset, coords)
      return(dataset)
}
mnbls_proc <- lapply(mnbls_proc, projUTM20)

##### Export #####
for(trans in names(mnbls_proc)){
      write_excel_csv(mnbls_proc[[trans]], paste0(sinkPath, trans,".csv"))
}



