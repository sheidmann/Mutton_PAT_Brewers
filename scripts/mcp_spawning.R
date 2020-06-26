# mcp_spawning.R

# Sarah Heidmann
# Created 26 Jun 2020
# Last modified 26 Jun 2020

# Summary: calculates size of MCP that includes spawning detections

# Data inputs:
#     - subsampled detections for MCP creation (45334 and 45338)
#     - spawning detections (45334 and 45338)
# Actions:
#     - combines subsampled detections with spawning ones
#     - creates 95% MCP for each and calculates the size
# Data exports:
#     - none

# Load the libraries
library(tidyverse)
library(adehabitatHR)

##### Import the data #####
# The subsamples are binned but the spawning detections are not.
# Since the scale is so large, position averaging doesn't really matter.
# I will just use the raw detections at the spawning sites to save time.

# Import subsamples
sourcePath_sub <- "data/mcp_subsample/"
filenames_sub <- list.files(path = sourcePath_sub) %>%
     subset(substr(.,10,14) %in% c("45334","45338"))
importMNB_sub <- function(filename, filepath){
     # Read the file
     dat <- read_csv(paste0(filepath, filename))
     # Return the dataset
     return(dat)
}
mnb_sub <- lapply(filenames, importMNB_sub, sourcePath_sub) # read the files into a list
names(mnb_sub) <- gsub(".csv", "", filenames) # take .csv out of the name
mnb_sub

# Import spawning detections
# The function above will still work, use same procedure
sourcePath_spawn <- "data/spawning/"
filenames_spawn <- list.files(path = sourcePath_spawn)
mnb_spawn <- lapply(filenames_spawn, importMNB_sub, sourcePath_spawn)
names(mnb_spawn) <- gsub(".csv","",filenames)
mnb_spawn
# Project spawning detections
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
mnb_spawn <- lapply(mnb_spawn, projUTM20)

##### Combine the datasets #####
m45334 <- mnb_sub[[1]] %>% 
     rename(x_UTM20N=avg_x,y_UTM20N=avg_y) %>%
     bind_rows(mnb_spawn[[1]])
m45338 <- mnb_sub[[2]] %>% 
     rename(x_UTM20N=avg_x,y_UTM20N=avg_y) %>%
     bind_rows(mnb_spawn[[2]])

##### Create the MCPs #####
makeMCP_spawn <- function(dataset, mcpPercent){
     # Extract transmitter ID
     trans <- dataset$transmitter[1]
     # set CRS projection
     crs_proj <- "+proj=utm +zone=20 +datum=NAD83 +units=m"
     # set coordinate columns
     coord_cols <- c("x_UTM20N","y_UTM20N")
     # Convert to SpatialPoints
     dat_sp <- SpatialPoints(coords = dplyr::select(dataset, coord_cols), 
                             proj4string = CRS(crs_proj))
     # Make the MCP
     dat_mcp <- adehabitatHR::mcp(dat_sp, percent = mcpPercent, unout = "m2")
     # How big is it? These are the same, no matter the projection
     mcpArea <- round(as.data.frame(dat_mcp)$area/1000000, 1)
     cat("\n", paste0(trans, " MCP Size (km^2): ", mcpArea),"\n")
     # Convert to data frame
     dat_mcp_f <- suppressMessages(fortify(dat_mcp)) %>%
          as_tibble() %>%
          dplyr::select(long, lat, order) %>%
          add_column(transmitter = trans)
     return(dat_mcp_f)
}

# For 45334, force to include the spawning detections by using 100%
mcp_45334 <- suppressMessages(makeMCP_spawn(m45334, mcpPercent = 100))
# 18 km2

# For 45336, use the regular 95%
mcp_45338 <- suppressMessages(makeMCP_spawn(m45338, mcpPercent = 95))
# 69.7 km2
