# transum.R

# Sarah Heidmann
# Created 20 Jan 2017
# Last modified 18 May 2020

# Summary: Creates a detection table for Brewers Bay mutton snapper

# Data inputs:
#     - mutton snapper acoustic data: 3_cut (exported from 2_cutting.R)
# Actions:
#     - summarizes fish metadata and detection info
#     - populates a table
# Data exports:
#     - A detection summary table by transmitter for detections in Brewers

# Load the libraries
library(tidyverse)
library(lubridate)

##### Enter parameters #####
# Enter the name of the transmitter master sheet (csv) with filepath
transmasterPath <- "data/otherdata/mnb_mutton_transmitter_master_2017.csv"

# Enter the name of the station master sheet (csv) with filepath
statmasterPath <- "data/otherdata/mnb_station_master_2017.csv"

# Enter the filepath for the source file folder (one csv per fish)
sourcePath <- "data/3_cut/"

# Enter the filepath for the final table
sinkPath <- "outputs/"

##### Run it #####
# Get the list of files
filenames <- list.files(path = sourcePath)

# Read the transmitter master sheet
transmaster <- read_csv(transmasterPath) %>%
   # Keep only needed columns
   select(transmitter, release_date, TL)

# Read the station master sheet
statmaster <- read_csv(statmasterPath)

# Create the empty detection table
transum <- tibble(Transmitter = character(),
                  TL.cm = numeric(),
                  TagDate = Date(),
                  No.detections = integer(),
                  First.detection = Date(),
                  Last.detection = Date(),
                  Days.between.first.and.last = integer(),
                  Days.detected = integer(),
                  No.stations.visited = integer())

# Fill the table
for (i in 1:length(filenames)){
      # Read each file
      temp <- read_csv(paste0(sourcePath, filenames[i]),
                       col_types=cols(station = col_character(),
                           detection_time_ast=col_datetime(format="%Y/%m/%d %H:%M:%S")))%>%
         # Set time zone
         mutate(detection_time_ast = force_tz(detection_time_ast, 
                                              "America/Virgin"))
      transInfo <- filter(transmaster, transmitter==temp$transmitter[1])
      first <- date(min(temp$detection_time_ast))
      last <- date(max(temp$detection_time_ast))
      # Create a row for the transmitter
      tempsum <- tibble(Transmitter = temp$transmitter[1],
                        TL.cm = select(transInfo, TL) %>% as.numeric(),
                        TagDate = transInfo$release_date,
                        No.detections = nrow(temp),
                        First.detection = first,
                        Last.detection = last,
                        Days.between.first.and.last = 
                           as.numeric(last - first + 1),
                        Days.detected = length(unique(temp$date)),
                        No.stations.visited = length(unique(temp$station)))
      transum <- bind_rows(transum, tempsum)
}

# Export the table
transum %>%
   arrange(TagDate, Transmitter) %>% # sorted by date tagged, then transmitter
   write_excel_csv(., paste0(sinkPath, "transum_mnb_muttonsnapper.csv"))
