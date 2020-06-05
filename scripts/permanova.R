# permanova.R
# Sarah Heidmann

# Created 8 Feb 2018
# Last modified 5 Jun 2020

# Summary:
# Research Question: Are L. analis changing movements daily or seasonally?
# Data inputs:
#     - mutton snapper acoustic data: 3_cut (exported from 2_cutting.R)
# Actions:
#     - make a community matrix for receiver presence over time
#     - run a repeated measures PERMANOVA by month and day
# Data exports:
#     - none (console output)

# Reference:
# https://thebiobucket.blogspot.com/2011/04/repeat-measure-adonis-lately-i-had-to.html#more

# Some notes on the setup:
# We are testing receiver station usage across months (seasonal) and days.
# H0: receiver usage composition is the same across time.
# Using the Jaccard index.

# For a balanced design, we are only using the time when 5 fish are overlapping.
# 24797, 45337, 45338, 45339, and 45334 from 11/3/16 to 6/10/17 (remove 52797)

# Monthly, we have 39 receiver sites at 7 time points (Nov-May) (273 rows)
# Daily, we have 39 receiver sites at 220 time points (17108-17327) (8580 rows)

# Load the libraries
library(tidyverse)
library(lubridate)
library(vegan)

##### Import the data #####
# Read the acoustic files
sourcePath <- "data/3_cut/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
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
mnb_ls # check it

# Load list of active Brewers receivers
statmaster <- read_csv("data/otherdata/mnb_station_master_2017.csv") %>%
        filter(station < 287) # remove the unused stations

##### Make the matrices #####
# Make master time lists
monthlist <- rep(c(11,12,1:5), 39) # for each station, list each month
startdate <- as.Date("2016-11-03") # first date to include
enddate <- as.Date("2017-06-10") # last date to include
datelist <- rep(seq(as.integer(startdate), # for each station, list each day
                    as.integer(enddate), by = 1), 39)
# Write a single function to create the community matrices by month and day.
PERMANOVA_setup <- function(dataset, time_unit){
        # Initial data setup
        dataset_sub <- dataset %>%
                select(station, transmitter, date) %>% # columns of interest
                mutate(transmitter=gsub("A69-1601-","m",
                                        transmitter)) %>% # shorten transmitter name
                filter(date >= startdate & date <= enddate) # only use period of interest
        # Set a few variables depending on whether testing month or day
        if(time_unit=="month"){
                stat_v <- rep(as.character(statmaster$station), 
                              each = 7) # make station list
                time_v <- monthlist # make month list
                dataset_sub <- dataset_sub %>% 
                        mutate(timeperiod = month(date)) # extract the month
        } else if(time_unit=="day"){
                stat_v <- rep(as.character(statmaster$station), 
                              each = 220) # make station list
                time_v <- datelist # set date list
                dataset_sub <- dataset_sub %>% 
                        mutate(timeperiod = as.integer(date))
        }
        # Make the community matrix
        pres_sum <- dataset_sub %>%
                group_by(transmitter, timeperiod, station) %>%
                summarise(pres=1) %>% ungroup() %>% # set presence value
                pivot_wider(id_cols=c("station","timeperiod"), # for unique station-times
                            names_from = "transmitter", # each transmitters has a column
                            values_from = "pres") %>% # filled with presence
                right_join(tibble(station=stat_v, # join to full station-time list
                                  timeperiod=time_v)) %>% 
                replace(is.na(.), 0) %>% # turn missing values into absence
                select(-timeperiod,-station) %>% # take out id cols
                add_column(dummy=1) # add a dummy fish so dissimilarity works
        return(pres_sum)
}
# First by month
month_pres <- mnb_ls[c(1,3,5:7)] %>% # Only use the 5 overlapping fish
        bind_rows() %>% # put them in one dataset
        PERMANOVA_setup("month") # apply the function
# Then by day
day_pres <- mnb_ls[c(1,3,5:7)] %>% # Only use the 5 overlapping fish
        bind_rows() %>% # put them in one dataset
        PERMANOVA_setup("day") # apply the function


##### Run the test# Test across months
mfit <- vegan::adonis(vegdist(month_pres, method = "jaccard") ~ 
                              monthlist, # use master month list
                      permutations = 1000)
mfit$aov.tab

# Test across days
dfit <- vegan::adonis(vegdist(day_pres, method = "jaccard") ~ 
                              datelist, # use master date list
                      permutations = 32) # large sample size so fewer needed


dfit$aov.tab
