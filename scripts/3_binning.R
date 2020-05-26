# 3_binning.R

# Sarah Heidmann
# Created 8 Oct 2018
# Last modified 18 May 2020

# Summary:
# Data inputs:
#     - mutton snapper acoustic data: 3_cut (exported from 2_cutting.R)
# Actions:
#     - bins detections into an average position every hour
# Data exports:
#     - 4_binned_1hr

# Load libraries
library(tidyverse)
library(lubridate)

##### Import data #####
sourcePath <- "data/3_cut/" # determine data location
filenames <- list.files(sourcePath) # extract filenames
importMNB <- function(filename){
   # Read the file
   dat <- read_csv(paste0(sourcePath, filename),
                   col_types = cols(station = col_character(),
                        detection_time_ast=col_datetime(format="%Y/%m/%d %H:%M:%S"),
                        timediff = col_integer())) %>%
      # Change time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, 
                                           "America/Virgin"))
   # Return the dataset
   return(dat)
}
mnbls <- lapply(filenames, importMNB) # read all the files into a list
names(mnbls) <- gsub(".csv", "", filenames) # take .csv out of the names
mnbls # check it

# Specify the location for the exported data
sinkPath <- "data/4_binned_1hr/"

##### Bin the data #####
# Create a function to lapply
posavg <- function(dataset){
   # Extract transmitter
   trans <- dataset$transmitter[1]
   # Bin the detections for hours when present
   datasetbin <- dataset %>% group_by(transmitter,date,hour) %>%
      summarize(No.stations = length(unique(station)), # number of unique stations
                No.detections = length(station), # total number of detections
                daynight = names(which.max(table(daynight))), # period set to most common value
                # average all position columns
                avg_lat = round(mean(lat_nad83),5), 
                avg_long = round(mean(long_nad83),5),
                avg_x = round(mean(x_UTM20N),2),
                avg_y = round(mean(y_UTM20N),2)) %>%
      ungroup()
   # Add the zeros for hours when not present
   # For each date in the range of the dataset
   for(idate in range(dataset$date)[1]:range(dataset$date)[2]){
      idate <- as.Date(idate, origin = "1970-01-01") # make it usable
      # For each hour of the day
      for(ihour in 0:23){
         # Subset the data
         datasetsub <- filter(dataset,date==idate & hour==ihour)
         # If no detections, add row with 0s and NAs
         if(nrow(datasetsub)==0){ # if no detections, put 0s and NAs
            datasetbin <- datasetbin %>%
               add_row(tibble_row(transmitter=trans, date=idate,hour= ihour,
                                      No.stations=0, No.detections=0, 
                                      avg_lat=NA, avg_long=NA, avg_x=NA, avg_y=NA))
         }
      }
   }
   # Delete detections after last hour on last day
   datasetbin <- filter(datasetbin, 
                        !(date==range(dataset$date)[2] & 
                             hour>hour(range(dataset$detection_time_ast)[2]))) %>%
      arrange(date, hour) # sort it
   # Return the result
   return(datasetbin)
}
mnbls_bin <- lapply(mnbls, posavg)

lapply(mnbls, nrow)
lapply(mnbls_bin, nrow)

##### Calculate hourly residence index #####
calcHRI <- function(dataset) {
   hri <- (nrow(dataset) - sum(is.na(dataset$avg_lat))) / nrow(dataset)
   hri <- round(hri,2) # round the result
   return(hri)
}
lapply(mnbls_bin, calcHRI)


##### Export #####
# Only export those with sufficient data
for(dataset in mnbls_bin){
   trans <- dataset$transmitter[1] # dynamically name the file
   if(!(substr(trans, 10,14) %in% c("36029","45336"))){ # exclude those with insufficient data
      write_excel_csv(dataset, paste0(sinkPath, trans, ".csv"))
   }
}
