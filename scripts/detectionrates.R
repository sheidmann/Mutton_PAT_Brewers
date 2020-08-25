# plotrecdetday.R

# Sarah Heidmann
# Created 3 Jun 2020
# Modified 14 Aug 2020

# Summary:
# Data inputs:
#     - 4_binned_1hr (exported from 3_binning.R)
# Actions:
#     - calculates detection rates during day, night, and crepuscular periods
#     - compares station visitation rates between day and night with a t-test
#     - compares detection rates between day and night with a t-test
#     - tests correlation between station visitation rates and detection rates
#     - compares station visitation rates between spawning and pre-spawning with a t-test
#     - compares detection rates between spawning and pre-spawning with a t-test
# Data exports:
#     - none (console output)

# Load libraries
library(tidyverse)
library(lubridate)

##### Import the data #####
sourcePath <- "data/4_binned_1hr/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
importMNB <- function(filename){
     # Read the file
     dat <- read_csv(paste0(sourcePath, filename))
     # Return the dataset
     return(dat)
}
mnb <- lapply(filenames, importMNB) %>% # read all the files into a list
     bind_rows() # combine them into one dataset for testing
mnb # check it

##### Summarize detection rates for day and night #####
# Summary stats for each period
mnb %>%
     group_by(daynight) %>%
     summarise(periods = length(No.detections),
               meanstats = mean(No.stations),
               sestats = sd(No.stations)/sqrt(length(No.detections)),
               meandets = mean(No.detections),
               sedets = sd(No.detections)/sqrt(length(No.detections)))
# Fewer crepuscular periods, and higher variation

# Plot detections by hour of the day
mnb %>%
        group_by(hour) %>%
        summarise(meanstats = mean(No.stations),
                  sestats = sd(No.stations)/sqrt(length(No.stations)),
                  meandets = mean(No.detections),
                  sedets = sd(No.detections)/sqrt(length(No.detections))) %>%
        ggplot(data=.) +
        # geom_point(aes(x=hour,y=meandets)) +
        # geom_errorbar(aes(x=hour,ymin=meandets-sedets,ymax=meandets+sedets)) +
        geom_point(aes(x=hour,y=meanstats)) +
        geom_errorbar(aes(x=hour,ymin=meanstats-sestats,ymax=meanstats+sestats))
        

##### Test between day and night #####

# Did they visit  stations at different rates during the night and day?
# They are paired because we're matching them to a date

# Make the dataset
daynight_stat <- mnb %>% 
        pivot_wider(id_cols=c(transmitter,date), names_from = daynight,
                    values_from = No.stations, 
                    values_fn = list(No.stations = mean)) # calc stations/hour
# Check for normality
shapiro.test(daynight_stat$day) # not normal (p<0.001)
shapiro.test(daynight_stat$night) # not normal (p<0.001)
# A t-test is robust to lack of normality with a large enough sample size.
# Because my sample unit is day, sample size is pretty large (n=1898)
# Test for equal variance
var.test(daynight_stat$day, daynight_stat$night)
# Variances are equal p=0.12

# Test hypothesis that night stations != day stations
t.test(daynight_stat$day, daynight_stat$night, 
       paired = TRUE, var.equal = TRUE)
# Significant at p<0.001


# Did they have different numbers of detections during the night and day?
# They are paired because we're matching them to a date

# Make the dataset
daynight_det <- mnb %>% 
        pivot_wider(id_cols=c(transmitter,date), names_from = daynight,
                    values_from = No.detections, 
                    values_fn = list(No.detections = mean)) # calc detections/hour
# Check for normality
shapiro.test(daynight_det$day) # not normal (p<0.001)
shapiro.test(daynight_det$night) # not normal (p<0.001)
# A t-test is robust to lack of normality with a large enough sample size.
# Because my sample unit is day, sample size is pretty large (n=1898)
# Test for equal variance
var.test(daynight_det$day, daynight_det$night)
# Variances are not equal p<0.001

# Test hypothesis that night detections != day detections
t.test(daynight_det$day, daynight_det$night, 
       paired = TRUE, var.equal = FALSE)
# Significant at p<0.001


##### Correlation between stations and detections #####
# Is the number of stations visited associated with the number of detections?
# Make the dataset
cor_det_stat <- mnb %>%
        group_by(transmitter, date) %>%
        summarise(meanstatperhour = mean(No.stations),
                  meandetperhour = mean(No.detections))
# Test for linearity
ggplot(data = cor_det_stat, 
       aes(x = meandetperhour, y = meanstatperhour)) +
        geom_point(aes(color = transmitter)) +
        geom_smooth(formula = y~x, method = "lm") +
        xlab("Number of detections per day") +
        ylab("Number of stations per day")
# This looks very linear within transmitters, and fairly linear across them.
# Groupy: low (24797 and 59271) vs high (rest)

# Test for normality of both variables
# First by looking:
hist(cor_det_stat$meandetperhour) # normal but with lots of zeros
hist(cor_det_stat$meanstatperhour) # almost-normal with lots of zeros

# We didn't meet the assumptions, so calculate r with Spearman's rank coefficient
cor.test(cor_det_stat$meandetperhour, cor_det_stat$meanstatperhour, 
         method = "spearman")
# Warning: cannot compute exact p-value with ties
# Try a jitter
cor.test(jitter(cor_det_stat$meandetperhour), jitter(cor_det_stat$meanstatperhour), 
         method = "spearman")
# Stations and detections are correlated (Spearman's rank, rho=0.93, p<0.0001).


##### Summarize detection rates across spawning periods #####
# Set spawning month categories
spawn_cats <- tibble(month=seq(1,12,1),
                     spawn_cat=c(rep("prespawn",3), # Jan-Mar are prespawn
                                 rep("spawn",5), # Apr-Aug are spawning
                                 rep(NA,3), #Sept-Nov are NA
                                 "prespawn")) # Dec is pre-spawn
# Categorize bins into spawning categories by month
mnb <- mnb %>% 
        mutate(month = month(date)) %>%
        left_join(spawn_cats)
# Summary stats for each period
mnb %>%
        group_by(spawn_cat) %>%
        summarise(periods = length(No.detections),
                  meanstats = mean(No.stations),
                  sestats = sd(No.stations)/sqrt(length(No.detections)),
                  meandets = mean(No.detections),
                  sedets = sd(No.detections)/sqrt(length(No.detections)))
# Fewer pre-spawn periods and similar variation
# Higher station and detection rates in pre-spawn period

# Plot detections by month
mnb %>%
        group_by(month, spawn_cat) %>%
        summarise(meanstats = mean(No.stations),
                  sestats = sd(No.stations)/sqrt(length(No.stations)),
                  meandets = mean(No.detections),
                  sedets = sd(No.detections)/sqrt(length(No.detections))) %>%
        ggplot(data=.) +
        geom_point(aes(x=month,y=meandets, color=spawn_cat)) +
        geom_errorbar(aes(x=month,ymin=meandets-sedets,ymax=meandets+sedets,
                          color=spawn_cat))
# Plot stations by month
mnb %>%
        group_by(month, spawn_cat) %>%
        summarise(meanstats = mean(No.stations),
                  sestats = sd(No.stations)/sqrt(length(No.stations)),
                  meandets = mean(No.detections),
                  sedets = sd(No.detections)/sqrt(length(No.detections))) %>%
        ggplot(data=.) +
        geom_point(aes(x=month,y=meanstats,color=spawn_cat)) +
        geom_errorbar(aes(x=month,ymin=meanstats-sestats,ymax=meanstats+sestats,
                          color=spawn_cat))


##### Test between spawn and pre-spawn #####

# Did they visit stations at different rates during pre-spawning and spawning?

# Make the dataset
spawn_stat <- mnb %>% 
        pivot_wider(id_cols=c(transmitter,date), names_from = spawn_cat,
                    values_from = No.stations, 
                    values_fn = list(No.stations = mean)) # calc stations/hour
# Check for normality
shapiro.test(spawn_stat$spawn) # not normal (p<0.001)
shapiro.test(spawn_stat$prespawn) # not normal (p<0.001)
# A t-test is robust to lack of normality with a large enough sample size.
# Because my sample unit is day, sample size is pretty large (n=1898)
# Test for equal variance
var.test(spawn_stat$spawn, spawn_stat$prespawn)
# Variances are not equal p<0.001

# Test hypothesis that spawn stations != prespawn stations
t.test(spawn_stat$spawn, spawn_stat$prespawn, 
       paired = FALSE, var.equal = FALSE)
# Significant at p<0.001


# Did they have different numbers of detections during spawn and pre-spawn?

# Make the dataset
spawn_det <- mnb %>% 
        pivot_wider(id_cols=c(transmitter,date), names_from = spawn_cat,
                    values_from = No.detections, 
                    values_fn = list(No.detections = mean)) # calc detections/hour
# Check for normality
shapiro.test(spawn_det$spawn) # not normal (p<0.001)
shapiro.test(spawn_det$prespawn) # not normal (p<0.001)
# A t-test is robust to lack of normality with a large enough sample size.
# Because my sample unit is day, sample size is pretty large (n=1898)
# Test for equal variance
var.test(spawn_det$spawn, spawn_det$prespawn)
# Variances are not equal p<0.001

# Test hypothesis that spawn detections != prespawn detections
t.test(spawn_det$spawn, spawn_det$prespawn, 
       paired = FALSE, var.equal = FALSE)
# Significant at p<0.001


# Did they have different hourly residency indices during spawn and pre-spawn?

# Make the dataset
spawn_res <- mnb %>%
     mutate(PA=ifelse(No.detections>0,1,0)) %>%
     pivot_wider(id_cols=c(transmitter,date, hour), names_from = spawn_cat,
                 values_from = PA) # calc detections/hour
# A t-test is robust to lack of normality with a large enough sample size.
# Because my sample unit is hour, sample size is HUGE
# Test for equal variance
var.test(spawn_res$spawn, spawn_res$prespawn)
# Variances are not equal p<0.001

# Test hypothesis that spawn detections != prespawn detections
t.test(spawn_res$spawn, spawn_res$prespawn, 
       paired = FALSE, var.equal = FALSE)
# Significant at p<0.001
# Does this hold when removing the fish that was detected elsewhere?
spawn_res_sub <- spawn_res %>%
     filter(transmitter != "A69-1601-45338")
t.test(spawn_res_sub$spawn, spawn_res_sub$prespawn, paired=FALSE, var.equal=FALSE)
# Yes, significant at p<0.001