# 2_cutting.R

# Sarah Heidmann
# Created 15 Oct 2018
# Last modified 18 May 2020

# Summary:
# Data inputs:
#     - mutton snapper acoustic data: 2_processed_SLH (exported from 2_processing.R)
#     - table of previously determined "Dead Dates"
# Actions:
#     - removes detections at other arrays
#     - cuts data after midnight on a conservative date (last time definitely alive)
#     - cuts the day of tagging and the first full day after tagging
#     - If the fish survived until the end, it cuts before the last array pull 
# Data exports:
#     - 3_cut

# Load libraries
library(tidyverse)
library(lubridate)

##### Import the data #####
# Read the files
sourcePath <- "data/2_processed_SLH/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
importMNB <- function(filename){
      # Read the file
      dat <- read_csv(paste0(sourcePath, filename),
                       col_types = cols(station = col_character())) %>%
         # Change time zone
         mutate(detection_time_ast = force_tz(detection_time_ast, 
                                              "America/Virgin"))
      # Return the dataset
      return(dat)
}
mnb2ls <- lapply(filenames, importMNB) # read all the files into a list
names(mnb2ls) <- gsub(".csv", "", filenames) # take .csv out of the names
# names(mnb2ls) # check it

# Check one file
# mnb2ls[[1]]
# tz(mnb2ls[[1]]$detection_time_ast)

# Make a list of the transmitters
transList <- names(mnb2ls) 

# Import the transmitter master file
transMaster <- read_csv("data/otherdata/uvi_transmitter_master_2017.csv",
                        col_types = 
                           cols(TL = col_double(),
                                release_date = col_date(format="%m/%d/%y"))) %>%
   filter(transmitter %in% transList)
# ignore 14 parsing failures

# Import the station master file
statmaster <- read_csv("data/otherdata/mnb_station_master_2017.csv",
                       col_types = cols(station=col_character()))

# Read the previously-determined dead dates
deadDates <- read_csv("data/otherdata/deaddates_20180117.csv")

# Enter the final array pull date (last date to keep)
finalArrayPullDate <- as.Date("2017-11-08")

# Enter the location for exported files
sinkPath <- "data/3_cut/"

##### Trim the data #####
# deadDate and arrayPullDate should be the last day you would like to keep
# Write a function to match DeadDates to fish and trim
trimPAT <- function(dataset, deadDates, arrayPullDate){
   # Save the transmitter ID
   trans <- dataset$transmitter[1]
   # Cut non-Brewers detections
   dataset <- filter(dataset, station %in% statmaster$station)
   # Print starting date range
   cat("Starting date range for transmitter ", trans, ": \n", 
       as.character(range(dataset$date)[1]), " to ", 
       as.character(range(dataset$date)[2]), "\n")
   # Cut beginning (remove through first full day after tagging)
   # AKA data start at the second midnight after tagging
   start <- filter(transMaster, transmitter == trans) %>%
      select(release_date) %>% # find the tag date
      pull + 1 # extract the day after the tag date
   dataset <- filter(dataset, date > start) # filter to start the next day
   # Cut end
   end <- select(deadDates, gsub("-",".",trans)) %>% # find the dead date
      pull # extract it
   # If a dead date exists, cut data after it
   if(!is.na(end)){
      dataset <- filter(dataset, date <= end)
      # If a dead date does not exist, cut before most recent array pull
   } else{
      end <- arrayPullDate
      dataset <- filter(dataset, date <= arrayPullDate)
   }
   # Print ending date range
   cat("Ending date range for transmitter ", trans, ": \n", 
       as.character(range(dataset$date)[1]), " to ", 
       as.character(range(dataset$date)[2]), "\n")
   # Return the dataset
   return(dataset)
}
# Apply to the list of datasets
mnb2ls_cut <- lapply(mnb2ls, trimPAT, deadDates, finalArrayPullDate)

##### Export the data #####
for(dataset in mnb2ls_cut){
   trans <- dataset$transmitter[1] # dynamically name the file
   write_excel_csv(dataset, paste0(sinkPath, trans, ".csv"))
}