# mcp_creation.R

# Sarah Heidmann
# Created 5 July 2017
# Last modified 25 May 2020

# Summary: Creates MCPs for Brewers Bay mutton snapper

# Data inputs (choose one):
#     - mutton snapper acoustic data: 4_binned_1hr (exported from 3_binning.R)
#     - subsampled acoustic data: mcp_subsample
#         - if this exists, skip the first section
# Actions:
#     - subsamples the datasets for consistent sample size
#     - creates 95% MCPs for full data
#     - creates 50% MCPs for full data
#     - creates 95% MCPs for day and night data
# Data exports:
#     - optional: subsampled acoustic data
#     - Fortified data frames of polygon vertices

# Load the libraries
library(tidyverse)
library(sp)
library(adehabitatHR)

##### Create the subsampled data #####
# Skip this section if they've already been created

# MCPs are sensitive to sample size
# We want to use the same number of points to make each fish's MCPs.
# Import the binned data
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

# Take random sample of points
# Subsample datasets so all are the same length as the shortest one
samplesize <- lapply(mnb_ls, nrow) %>% unlist() %>% min() # n=2770

subsampleMCP <- function(dataset, samplesize){
     # Select random rows out of the dataset
     dat_sub <- dataset %>%
          # Make unique id column out of rownames
          rownames_to_column('id') %>%
          # Select random rows out of the dataset
          filter(id %in% sample(1:nrow(dataset), samplesize, replace = FALSE)) %>%
          # Delete unique id column and convert back to tibble
          column_to_rownames('id') %>% as_tibble()
     return(dat_sub)
}

mnb_sub <- lapply(mnb_ls, subsampleMCP, samplesize = samplesize)
lapply(mnb_sub, nrow)

# Export for use later
for(dataset in mnb_sub){
      trans <- dataset$transmitter[1] # name based on transmitter
      write_excel_csv(dataset, paste0("data/mcp_subsample/", trans, ".csv"))
}

# You can skip the next section if you've completed this one.

##### Import the subsampled data #####
sourcePath_sub <- "data/mcp_subsample/"
filenames <- list.files(path = sourcePath_sub)
importMNB_sub <- function(filename, filepath){
     # Read the file
     dat <- read_csv(paste0(sourcePath_sub, filename))
     # Return the dataset
     return(dat)
}
mnb_sub <- lapply(filenames, importMNB_sub, sourcePath_sub) # read the files into a list
names(mnb_sub) <- gsub(".csv", "", filenames) # take .csv out of the name
mnb_sub

# Now we can run these smaller datasets through the functions below.

##### Create the MCPs #####
# Make a modifiable function with variable percent and time of day
# This way we can use one function to make:
#     - 95% MCPs for full data
#     - 50% MCPs for full data
#     - 95% MCPs for day data
#     - 95% MCPs for night data

makeMCP <- function(dataset, MCPpercent, subset = "all", 
                    proj = FALSE, sinkPath = NA){
   # Filter if desired
   if(subset == "all"){
      dat <- dataset
   } else if(subset == "day"){
      dat <- dataset %>% filter(daynight=="day")
   } else if(subset == "night"){
      dat <- dataset %>% filter(daynight=="night")
   } else{
      #cat("\n Invalid subset. Please enter 'all','day', or 'night'.")
      return(cat("\n Invalid subset. Please enter 'all','day', or 'night'."))
   }
   # Extract transmitter ID
   trans <- dat$transmitter[1]
   cat(trans)
   # set CRS depending on whether want lat/long or UTM
   crs_proj <- ifelse(proj, # check whether projection is wanted
                      "+proj=utm +zone=20 +datum=NAD83 +units=m", # projected
                      "+proj=longlat +datum=NAD83") # lat/long
   # set coordinate columns depending on whether want lat/long or UTM
   coord_cols <- if(proj){ # check whether projection is wanted
      c("avg_x","avg_y") # projected
   } else {
      c("avg_long", "avg_lat") # lat/long
   }
   # Convert to SpatialPoints
   dat_sp <- SpatialPoints(coords = dplyr::select(dat, coord_cols), 
                           proj4string = CRS(crs_proj))
   # Make the MCP
   dat_mcp <- adehabitatHR::mcp(dat_sp, percent = MCPpercent, unout = "m2")
   # How big is it? These are the same, no matter the projection
   mcpArea <- round(as.data.frame(dat_mcp)$area, 1)
   cat("\n", paste0(as.character(MCPpercent), "% MCP Size (m^2): ", mcpArea),"\n")
   # Convert to data frame
   dat_mcp_f <- suppressMessages(fortify(dat_mcp)) %>%
      as_tibble() %>%
      dplyr::select(long, lat, order) %>%
      add_column(transmitter = trans) %>%
      { if(proj){rename(., x=long,y=lat)} else{.}}
   # Export the polygon vertices
   if(!is.na(sinkPath)){
      write_excel_csv(dat_mcp_f, paste0(sinkPath, trans,".csv"))
   }
   return(dat_mcp_f)
}

# Make 95% full MCPs
mcp_full95 <- lapply(mnb_sub, suppressMessages(makeMCP), 
                     MCPpercent = 95, subset = "all", proj = TRUE,
                       sinkPath="data/mcp_95/")

# Make 50% full MCPs
mcp_full50 <- lapply(mnb_sub, makeMCP, 
                     MCPpercent = 50, subset = "all", proj = TRUE,
                     sinkPath="data/mcp_50/")

# Make 95% day MCPs
mcp_day <- lapply(mnb_sub, makeMCP, 
                  MCPpercent = 95, subset = "day", proj = TRUE,
                     sinkPath="data/mcp_day/")

# Make 95% night MCPs
mcp_night <- lapply(mnb_sub, makeMCP, 
                    MCPpercent = 95, subset = "night", proj = TRUE,
                  sinkPath="data/mcp_night/")
