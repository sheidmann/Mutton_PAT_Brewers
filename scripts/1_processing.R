# 1_processing.R

# Sarah Heidmann
# Created 8 Oct 2018
# Last modified 3 Jun 2020

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
      dplyr::select(station, transmitter, detection_time_ast, lat_nad83,long_nad83) %>%
      # Change time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, 
                                           "America/Virgin")) %>%
      # Sort by time
      arrange(detection_time_ast)
   # Return the dataset
   return(dat)
}
mnb_ls <- lapply(filenames, importMNB)
names(mnb_ls) <- gsub(".csv", "", filenames)
mnb_ls

sinkPath <- "data/2_processed_SLH/"

##### Fix the positive longitudes #####
negLong <- function(dataset){
   dataset %>% dplyr::select(long_nad83) %>% 
      summary() # 4 of them have detections with a positive longitude
   dataset <- dataset %>%
      mutate(long_nad83 = ifelse(long_nad83>0, # if longitude is positive
                                 long_nad83 * -1, # turn it negative
                                 long_nad83)) # otherwise leave it
   #dataset %>% dplyr::select(long_nad83) %>% summary() # check them
   return(dataset)
}
mnb_ls <- lapply(mnb_ls, negLong)

##### Delete spawning detections #####
# Load list of active Brewers receivers
statmaster <- read_csv("data/otherdata/mnb_station_master_2017.csv")

# Keep only detections on that list
delStat <- function(dataset){
      dat <- filter(dataset, station %in% statmaster$station)
      return(dat)
}
mnb_ls_proc <- lapply(mnb_ls, delStat)

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
mnb_ls_proc <- lapply(mnb_ls_proc, splitDate)

##### Calculate time differences #####
calcTimeDiff <- function(dataset){
   # Subtract time
      dataset <- dataset %>%
         mutate(timediff= c(as.numeric(tail(detection_time_ast, -1)) -
                                  as.numeric(head(detection_time_ast, -1)), NA))
      print("Time difference calculation complete")
      return(dataset)
}
mnb_ls_proc <- lapply(mnb_ls_proc, calcTimeDiff)

##### Classify day/night #####
suntab <- read_csv("data/otherdata/sttsuntable_14_18.csv", 
                   col_types = cols(rise = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
                                    set = col_datetime(format = "%Y/%m/%d %H:%M:%S"))) %>%
   # Change time zone
   mutate(rise = force_tz(rise, "America/Virgin"),
          set = force_tz(set, "America/Virgin"))
addDayNight <- function(dataset){
   dataset <- left_join(dataset, suntab, by="date")
   dataset <- dataset %>%
      mutate(daynight=case_when(detection_time_ast<rise-3600 | detection_time_ast>set+3600~"night",
                                detection_time_ast>rise+3600 & detection_time_ast<set-3600~"day",
                                (detection_time_ast>=rise-3600 & detection_time_ast<=rise+3600) |
                                   (detection_time_ast>=set-3600 & detection_time_ast<=set+3600)~"crep")) %>%
      select(-rise, -set)
   print("Day/night classification complete")
   return(dataset)
}
mnb_ls_proc <- lapply(mnb_ls_proc, addDayNight)

# add UTM20 positions (x_UTM20 and y_UTM20) so don’t have to project every time
projUTM20 <- function(dataset){
      dataset_s <- SpatialPoints(coords = dplyr::select(dataset, long_nad83, lat_nad83),
                                 proj4string = CRS("+proj=longlat +datum=NAD83"))
      #project (UTM Zone 20N)
      dataset_sp <- spTransform(dataset_s, CRS("+proj=utm +zone=20 +datum=NAD83 +units=m"))
      coords <- as_tibble(dataset_sp) %>%
         rename(x_UTM20N=long_nad83, y_UTM20N=lat_nad83)
      dataset <- bind_cols(dataset, coords)
      return(dataset)
}
mnb_ls_proc <- lapply(mnb_ls_proc, projUTM20)

##### Export #####
for(trans in names(mnb_ls_proc)){
      write_excel_csv(mnb_ls_proc[[trans]], paste0(sinkPath, trans,".csv"))
}



