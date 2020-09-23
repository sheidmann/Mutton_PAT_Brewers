# plotrecdetday.R

# Sarah Heidmann
# Created 27 Jun 2017
# Modified 26 Jun 2020

# Summary:
# Data inputs:
#     - 3_cut (exported from 2_cutting.R)
#     - HOBO temperature data
# Actions:
#     - creates a plot showing activity over time for each fish
#     - By: receivers visited per day and number of detections per day
#     - and a plot of average temperature over that same time period
# Data exports:
#     - One plot of stations, detections, and temperature each day, colored by transmitter

# Load libraries
library(tidyverse)
library(lubridate)

##### Import the data #####
# Read the files
sourcePath <- "data/3_cut/" # source is modifiable
filenames <- list.files(sourcePath) %>% # extract the filenames
   subset(!(substr(.,10,14) %in% c("36029","45336"))) # remove unused fish
importMNB <- function(filename){
      # Read the file
      dat <- read_csv(paste0(sourcePath, filename),
                      col_types = cols(station = col_character(),
                              detection_time_ast=col_datetime(format="%Y/%m/%d %H:%M:%S"))) %>%
      # Change time zone
         mutate(detection_time_ast = force_tz(detection_time_ast, 
                                                 "America/Virgin"))
      # Return the dataset
      return(dat)
}
mnb_ls <- lapply(filenames, importMNB) # read all the files into a list
names(mnb_ls) <- gsub(".csv", "", filenames) # take .csv out of the names
mnb_ls # check data

# Import the temperature values
hobo <- read_csv("data/otherdata/mnb_temp_all_201410_201711.csv") %>%
   # Change time zone
   mutate(sampletime_15min = force_tz(sampletime_15min, "America/Virgin")) %>%
   # add date column
   mutate(date = as.Date(sampletime_15min)) %>%
   # Round the temperature value for consistency
   mutate(temp_c = round(temp_c,1))
# Check it
hobo

# Enter the location for exported plot
sinkPath <- "outputs/"

# Create a color list for plotting (colorblind friendly)
# For all 8 fish
# # dark blue, pink, orange, grey, yellow, green, light blue, red
# mnbColors <- c("#0072B2", "#CC79A7", "#E69F00", "#999999",
#                "#F0E442", "#009E73", "#56B4E9", "#D55E00")
# For 6 used fish
# dark blue, orange, yellow, green, light blue, red
mnbColors <- c("#0072B2", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#D55E00")
mnbColors <- setNames(mnbColors, substr(names(mnb_ls),10,14)) # link to transmitters

##### Summarize data by date #####
# Plot number of detections and receivers per day over time
sumRecDetDay <- function(dataset){
      # Summarize stations and detections by day and merge them
      recDetDay <- dataset %>% 
         group_by(date) %>%
         summarize(No.stations = length(unique(station)),
                   No.detections = length(station), .groups="drop")
      # Add zeroes for missing days within the range of the dataset
      for(idate in range(dataset$date)[1]:range(dataset$date)[2]){
         idate <- as.Date(idate, origin = "1970-01-01") # make it usable
         temp <- filter(recDetDay, date==idate) # find data for that day
         if(nrow(temp)==0){ # if no detections
            recDetDay <- recDetDay %>% # Add a row saying so
               add_row(tibble_row(date=idate, No.stations=0, No.detections=0))
         }
      }
      # Melt for plotting
      recDetDay <- recDetDay %>%
         pivot_longer(-date, names_to = "type", values_to = "value") %>%
         mutate(type= ifelse(type =="No.stations", "Stations per day", 
                             "Detections per day")) %>% # humanize labels
         arrange(date, type) %>% #sort by date
         add_column(transmitter = substr(dataset$transmitter[1],10,14))
      return(recDetDay)
}
# Create and bind all
recDetDay_all <- lapply(mnb_ls, sumRecDetDay) %>%
   bind_rows()

# Summarize and add temperature
recDetDay_all <- hobo %>% # start with raw
   add_column(type="Temperature (°C)") %>% # set type
   group_by(date, type) %>% 
   summarize(value = mean(temp_c), .groups="drop") %>% # mean temp per day
   filter(date >= range(recDetDay_all$date)[1] & 
             date <= range(recDetDay_all$date)[2]) %>% # use only desired period
   add_column(transmitter = as.character(NA)) %>% # add column for binding
   bind_rows(recDetDay_all) # bind to recDetDay

##### Summary statistics #####
# What was the range of stations visited per day?
recDetDay_all %>%
   filter(type == "Stations per day") %>%
   select(value) %>% range()

# By transmitter, what was the average stations visited per day?
recDetDay_all %>%
   filter(type == "Stations per day") %>%
   group_by(transmitter) %>%
   summarise(meanstats=mean(value, na.rm=TRUE), .groups="drop")

# What was the residency index?
recDetDay_all %>%
   filter(type == "Detections per day") %>%
   group_by(transmitter) %>%
   # number of days with detections / number of days
   summarise(RI = sum(value!=0) / length(value), .groups="drop")


##### Plot #####
ggplot(data = recDetDay_all) + 
   # Transmitter lines
   geom_line(aes(x=date, y=value, color=transmitter), 
             data = filter(recDetDay_all, !is.na(transmitter))) + 
   # Temperature lines
   geom_line(aes(x=date, y=value), color = "black", 
             data = filter(recDetDay_all, is.na(transmitter))) +
   # denote PERMANOVA period
   geom_vline(xintercept=as.Date(c("2016-11-03","2017-06-10")),
              linetype = "dashed", color = "black") +
   # labels
   xlab("Date") +
   ylab("") +
   # split stations, detections, and temperature
   facet_wrap(~type, scales = "free_y", ncol=1, strip.position = "left") + 
   # change x labels
   scale_x_date(date_breaks = "1 month", 
                date_labels = "%Y-%b") +
   # set the colors
   scale_color_manual("",values=c(mnbColors),
                      guide = guide_legend(nrow=1, override.aes=list(size=5))) +
   # formatting
   theme(axis.text.x = element_text(angle = -90), 
         text = element_text(size=16, family = "Times New Roman"),
         panel.background = element_blank(), 
         axis.line = element_line(),
         legend.key = element_blank(), 
         legend.position = "top",
         strip.background = element_blank(),
         strip.placement = "outside")
# Save the plot
# ggsave(filename = paste0(sinkPath, "tempRecDetDay_cut.jpeg"), 
#        width = 10, height = 7)

# PDF for publication
library(extrafont)
ggsave(filename = paste0(sinkPath, "Figure 2.pdf"), dpi=300, 
       width = 10, height = 7)
embed_fonts(paste0(sinkPath, "Figure 2.pdf"), 
            outfile=paste0(sinkPath, "Figure 2 embed.pdf"))
