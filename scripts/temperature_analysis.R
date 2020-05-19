#temperature_analysis.R

# Sarah Heidmann
# Created 5 May 2020
# Modified 7 May 2020

# Summary:
# Research Question: Are L. analis selecting for specific temperature ranges?
# Data inputs:
#     - mutton snapper acoustic data: 3_cut (exported from 2_cutting.R)
#     - HOBO logger temperature readings
#     - a table matching HOBO stations to receiver stations
# Actions:
#     - bins acoustic and temperature data into 30 min bins
#     - converts acoustic data to presence/absence at temperature stations
#     - plots overlaid histograms of acoustic and temperature data
# Data exports:
#     - histogram plots

# Frequency analysis of available temperatures vs species occurrence
# Suggested by Melissa Kimble

library(tidyverse)
library(lubridate)

##### Set up the temp data #####
# Start with temperature
hobo <- read_csv("data/otherdata/mnb_temp_all_201410_201711.csv") %>%
   # Change time zone
   mutate(sampletime_15min = force_tz(sampletime_15min, "America/Virgin")) %>%
   # Round the temperature value for consistency
   mutate(temp_c = round(temp_c,1))
hobo

# Match stations to receivers
unique(hobo$station)
hoboloc <- read_csv("data/otherdata/mnb_temp_locations.csv",
                    col_types = cols(closest_receiver = col_character()))

# Bin the temp data into 30 min bins (average two readings for each)
hobo$timebin <- substr(hobo$sampletime_15min,1,13) %>%
      paste0(.,ifelse(substr(hobo$sampletime_15min,15,16)<30,"A","B"))
hobo_bin <- hoboloc %>% 
      select(station,closest_receiver) %>%
      right_join(hobo, by="station") %>%
      group_by(timebin, closest_receiver) %>%
      summarize(meantemp_c=mean(temp_c), ntemp=length(temp_c)) %>%
   rename(station = closest_receiver)
# ggplot(aes(x=timebin,y=meantemp_c,color=closest_receiver),data=hobo_bin) +
#       geom_point()

# Average 2016 data
hobo_bin %>%
   add_column(year=substr(hobo_bin$timebin,1,4)) %>%
   filter(year == "2016") %>%
   ungroup() %>%
   summarise(meant = mean(meantemp_c), 
             sdt = sd(meantemp_c), nt = length(meantemp_c),
             mint = min(meantemp_c), maxt = max(meantemp_c)) %>%
   mutate(set = sdt / sqrt(nt))
# 28.6 +- 0.002 C (mean +- sem)

##### Set up acoustic data #####
# Read the files
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
# names(mnb_ls) # check it

# Convert into presence/absence for each temperature station for each time bin
#     - absence means there were detections at other receivers
#     - time bins with no detections were not included

# Bin acoustic data
bin_mnb_temp <- function(dat){
   # Take only relevant columns
   dat <- select(dat, station, transmitter, detection_time_ast)
   # Create half-hour time bins (A=0-29 min, B=30-59 min)
   dat$timebin <- substr(dat$detection_time_ast,1,13) %>%
      paste0(.,ifelse(substr(dat$detection_time_ast,15,16)<30,"A","B"))
   # Make a list of all the time bins when detected
   allbins <- select(dat, timebin) %>%
      unique()
   # Make a presence-only dataset by collapsing the raw
   dat2 <- dat %>%
      filter(station %in% hoboloc$closest_receiver) %>% # only temp stations
      group_by(timebin,transmitter,station) %>% # find unique combos
      summarise(presence = 1) # add presence column
   # Add in the absences
   final <- left_join(allbins, dat2) %>% # use all time bins list as reference
      fill(transmitter) %>% # all rows have transmitter value
      replace_na(replace=list(presence=0)) %>% # all rows have presence value
      complete(nesting(timebin,transmitter), # using only time bins that exist
               station, fill=list(presence=0)) %>% # complete with zeros
      filter(!is.na(station)) # take out NA stations
   return(final)
}
# Apply to list of datasets
mnb_ls_bin <- lapply(mnb_ls, bin_mnb_temp)

# Combine all transmitters
mnb_bin <- bind_rows(mnb_ls_bin)

##### Join them #####
# Join acoustic data to temp data (inner join keeps only matching values)
full <- inner_join(mnb_bin, hobo_bin, by=c("timebin", "station"))

##### Plot #####
# Scaled to probability density
# All together
alldensplot <- ggplot() +
   geom_histogram(aes(x=meantemp_c,y=..density..), # temperature hist
                  data = full, 
                  binwidth = 0.1, # force bins to 1 sig fig
                  fill = "red", alpha = 0.3) +
   geom_histogram(aes(x=meantemp_c,y=..density..), # presence hist
                  data = filter(full,presence==1), # only plot presence
                  binwidth = 0.1, # force bins to 1 sig fig
                  fill = "blue", alpha = 0.3)
facetdensplot <- alldensplot + facet_wrap(~transmitter, ncol=2) # facet by individual

##### Test #####
# First all together
# extract the y-values of the histogram bars
alldensdf <- ggplot_build(alldensplot)
tempy <- alldensdf$data[[1]]$y # these are the y-values for temperature
presy <- alldensdf$data[[2]]$y # these are the y-values for presence
# Kolmogorov Smirnov test
ks.test(presy,tempy, alternative = "two.sided", exact = FALSE)
# Two-sample Kolmogorov-Smirnov test
# 
# data:  presy and tempy
# D = 0.17333, p-value = 0.2099
# alternative hypothesis: two-sided
# 
# Warning message:
#    In ks.test(presy, tempy, alternative = "two.sided", exact = FALSE) :
#    p-value will be approximate in the presence of ties

# Now by individual
facetdensdf <- ggplot_build(facetdensplot)
temp_facet <- facetdensdf$data[[1]]
pres_facet <- facetdensdf$data[[2]]
for(i in 1:8){
   temp <- temp_facet[temp_facet$PANEL==i,]$y
   pres <- pres_facet[pres_facet$PANEL==i,]$y
   print(ks.test(pres,temp,alternative="two.sided", exact=FALSE))
}
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.18667, p-value = 0.1465
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.21333, p-value = 0.06586
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.13333, p-value = 0.5176
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.10667, p-value = 0.787
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.08, p-value = 0.97
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.14667, p-value = 0.3953
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.093333, p-value = 0.8996
# alternative hypothesis: two-sided
# 
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  pres and temp
# D = 0.066667, p-value = 0.9963
# alternative hypothesis: two-sided