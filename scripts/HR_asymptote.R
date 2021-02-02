# HR_asymptote.R

# Sarah Heidmann
# Created 26 Jan 2021
# Last modified 27 Jan 2021

# Summary: Did Brewers Bay mutton snapper reach asymptotic home range size?

# Data input:
#     - mutton snapper acoustic data: 4_binned_1hr (exported from 3_binning.R)
# Actions:
#     - subsamples the datasets in 10-day increments
#     - calculates 95% MCPs for each length of time
#     - calculates 95% BBMMs for each length of time 
#     - plots HR size over time
# Data exports:
#     - table of HR size over time
#     - plot of HR size over time

# Load the libraries
library(tidyverse)
library(adehabitatLT)
library(sp)
library(adehabitatHR)

##### Import the data #####
sourcePath <- "data/4_binned_1hr/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
importMNB <- function(filename){
     # Read the file
     dat <- read_csv(paste0(sourcePath, filename)) %>%
          # Remove time bins with no detections
          filter(No.detections>0)
     # Return the dataset
     return(dat)
}
mnb_ls <- lapply(filenames, importMNB) # read all the files into a list
names(mnb_ls) <- gsub(".csv", "", filenames) # take .csv out of the names
mnb_ls # check it

# Set a location to save the outputs
sinkPath <- "outputs/"

##### Function: cut the data #####
# Make a modifiable function to cut the data into each time period
cutTime <- function(dataset, numdays){
     # Find the first and last days
     firstday <- min(dataset$date) 
     lastday <- firstday + numdays - 1
     # Cut the data
     subdat <- dataset %>%
          filter(date <= lastday)
     # Return cut data
     return(subdat)
}

##### Set the BBMM parameters #####
# Using the same parameters for each fish in full BBMMs
codespace <- "A69-1601-"
BBMMparams <- tibble::tibble(ID=paste0(codespace, "24797"),
                        sig2_est = 72, 
                        sig1_est = 0.05) %>%
     bind_rows(tibble::tibble(ID=paste0(codespace, "45334"),
                              sig2_est = 62, 
                              sig1_est = 0.12)) %>%
     bind_rows(tibble::tibble(ID=paste0(codespace, "45337"),
                              sig2_est = 78, 
                              sig1_est = 0.12)) %>%
     bind_rows(tibble::tibble(ID=paste0(codespace, "45338"),
                              sig2_est = 52, 
                              sig1_est = 0.11)) %>%
     bind_rows(tibble::tibble(ID=paste0(codespace, "45339"),
                              sig2_est = 43, 
                              sig1_est = 0.34)) %>%
     bind_rows(tibble::tibble(ID=paste0(codespace, "59271"),
                              sig2_est = 55, 
                              sig1_est = 0.74))
# Function: convert data to ltraj
# This section is copied from my other script, bbmm_creation.R
# https://cran.r-project.org/web/packages/adehabitatHR/adehabitatHR.pdf
# To use kernelbb() you need an object of class ltraj (adehabitatLT)
# Acoustic data is type II trajectory (has times of relocations)
makeltraj <- function(dataset){
     # Convert to dataframe
     dataset <- as.data.frame(dataset)
     # Create unified datetime column (POSIXct)
     dataset$datehour <- as.POSIXct(paste(as.character(dataset$date),
                                          as.character(dataset$hour),
                                          sep = " "), 
                                    format = "%Y-%m-%d %H",
                                    tz= "America/Virgin")
     # Convert to ltraj
     dat_lt <- as.ltraj(dataset[c("avg_x", "avg_y")],
                        date = dataset$datehour,
                        id = dataset$transmitter,
                        typeII = TRUE,
                        proj4string = CRS("+proj=utm +zone=20 +datum=NAD83 +units=m"))
     # Some ways to look at it
     #dat_lt
     #summary.ltraj(dat_lt)
     #head(datasetltraj[[1]])
     #plot(dat_lt)
     
     # Return
     return(dat_lt)
}

##### Functions: calculate the homeranges #####
HRsize <- function(dataset, HRpercent=95, 
                   paramTab = NA, sinkTab = NA){
     # Extract transmitter ID
     trans <- dataset$transmitter[1]
     # Set projection
     crs_proj <- "+proj=utm +zone=20 +datum=NAD83 +units=m"
     # Convert to SpatialPoints
     dat_sp <- SpatialPoints(coords=dplyr::select(dataset, 
                                                  c("avg_x","avg_y")), 
                             proj4string = CRS(crs_proj))
     # Make the MCP
     dat_mcp <- adehabitatHR::mcp(dat_sp, percent = HRpercent, 
                                  unout = "m2")
     mcpArea <- round(as.data.frame(dat_mcp)$area, 1)
     # Make the BBMM
     dat_lt <- makeltraj(dataset)
     params <- filter(paramTab, ID==trans)
     bbmm <- kernelbb(dat_lt, 
                      sig1 = params$sig1_est, 
                      sig2 = params$sig2_est, 
                      grid = 50)
     bbmmArea <- as.integer(adehabitatHR::kernel.area(bbmm, 
                                           percent = HRpercent, 
                                           unout = "m2"))
     # Make tibble row
     tabrow <- tibble::tibble(ID = trans,
                             timedays = length(unique(dataset$date)),
                             MCPsizem2 = mcpArea,
                             BBMMsizem2 = bbmmArea)
     # If table given, return the row in the table
     if(is_tibble(sinkTab)){
          fulltab <- bind_rows(sinkTab, tabrow)
          return(fulltab)
     }
     # Otherwise, return just the row
     return(tabrow)
}
##### Make the blank table #####
hrtab <- tibble::tibble(ID = character(),
                        timedays = integer(),
                        MCPsizem2 = double(),
                        BBMMsizem2 = double())

##### Do the calculations #####
# HRsize(cutTime(mnb_ls[[1]], numdays=10), paramTab = BBMMparams, sinkTab=hrtab)
for(dataset in mnb_ls){
     maxdays <- length(unique(dataset$date))
     for(numdays in seq(10, maxdays, 10)){
          hrtab <- HRsize(cutTime(dataset, numdays = numdays),
                          HRpercent=100,
                  paramTab = BBMMparams, sinkTab=hrtab)
     }
}
# Convert to km2
hrtab <- hrtab %>%
     mutate(MCPsizekm2 = MCPsizem2 / 1000000,
            BBMMsizekm2 = BBMMsizem2 / 1000000)

hrtab

# Save it
# write_excel_csv(hrtab, paste0(sinkPath,"AsymptoticHomeRange.csv"))


##### Plot #####
# Melt for plotting
hrtab_melt <- hrtab %>%
     dplyr::select(ID, timedays, MCPsizekm2, BBMMsizekm2) %>%
     pivot_longer(cols = c(ends_with("km2")),
                  names_to = "Type",
                  values_to = "sizekm2") %>%
     mutate(Type = gsub("sizekm2","",Type)) %>%
     mutate(group = paste(ID, Type))
# Plot
ggplot(data = hrtab_melt) +
     geom_point(aes(x=timedays,y=sizekm2, color = ID, shape=Type)) +
     geom_line(aes(x=timedays,y=sizekm2, color = ID, group = group)) +
     xlab("Time tracked (days)") +
     ylab("Home range size (km2)") +
     theme(panel.background = element_blank(),
           axis.line = element_line())
# ggsave(filename = paste0(sinkPath, "AsymptoticHomeRange_100.jpeg"),
#        width = 10, height = 7)
