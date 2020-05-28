# mcp_metrics.R

# Sarah Heidmann
# Created 22 Jan 2018
# Last modified 26 May 2020

# Summary: calculates metrics of MCPs for Brewers Bay mutton snapper

# Data inputs (choose one):
#     - data frames of MCP polygon vertices
#         - 95% full, 50% full, 95% day, 95% night
# Actions:
#     - calculates size of each
#     - calculates overlap index (OI) of 50% full across all
#     - calculates OI and centroid separation of day/night for each
# Data exports:
#     - table of full 95% and 50% MCP sizes
#     - table of each fish's day/night OI and centroid separation

# Load the libraries
library(tidyverse)
library(sp)
library(raster)
library(rgeos)

##### Import the data #####
# Find the data
sourcePaths <- paste0("data/",c("mcp_95","mcp_50","mcp_day","mcp_night"),"/")
filenames <- list.files(sourcePaths[1])
# Read the data
readMCP <- function(filename, sourcePath){
     dat <- read_csv(paste0(sourcePath, filename))
     return(dat)
}
mcp_95 <- lapply(filenames, readMCP, sourcePaths[1])
names(mcp_95) <- gsub(".csv","", filenames)
mcp_95

mcp_50 <- lapply(filenames, readMCP, sourcePaths[2])
names(mcp_50) <- gsub(".csv","", filenames)
mcp_50

mcp_day <- lapply(filenames, readMCP, sourcePaths[3])
names(mcp_day) <- gsub(".csv","", filenames)
mcp_day

mcp_night <- lapply(filenames, readMCP, sourcePaths[4])
names(mcp_night) <- gsub(".csv","", filenames)
mcp_night

# Set the output location
sinkPath <- "outputs/"

##### Full MCP metrics #####
# For each of: 95% and 50% MCPs
#    - area

# Create a function to convert to SpatialPolygons and calculate area
calcMCPsize <- function(dataset, level = "", export){
     trans <- dataset$transmitter[1] # extract the transmitter
     proj <- ifelse(names(dataset)[1]=="x", TRUE, FALSE) # check if projected
     crs_proj <- ifelse(proj, # check projection
                        "+proj=utm +zone=20 +datum=NAD83 +units=m", # projected
                        "+proj=longlat +datum=NAD83") # lat/long
     coord_cols <- if(proj){ # check whether projection is wanted
          c("x","y") # projected
     } else {c("long", "lat")} # lat/long
     # Convert to SpatialPolygons
     sp<-SpatialPolygons(list(Polygons(list(Polygon(coords=
                                   dplyr::select(dataset, coord_cols))),1)),
                                  proj4string = CRS(crs_proj))
     # Calculate the area
     area <- raster::area(sp)
     # Create the table
     table <- tibble::tibble(Transmitter = trans,
                             Level = level,
                             Size.m2 = area)
     # Export either the table of sizes or the SpatialPolygons object
     if(export=="table"){
          return(table)
     } else if(export=="sp"){return(sp)}
     
}
# Create the full home range table
FullTable <- tibble::tibble(Transmitter = character(),
                            Level = character(),
                            Size.m2 = double())
FullTable <- lapply(mcp_95 ,calcMCPsize, level="95", export="table") %>% 
     bind_rows(FullTable)
FullTable <- lapply(mcp_50, calcMCPsize, level="50", export="table") %>% 
     bind_rows(FullTable)
FullTable <- FullTable %>% 
     arrange(Transmitter, desc(Level)) # Sort the output
# Export full home range table
write_excel_csv(FullTable, paste0(sinkPath, "MCP_Full_sizes.csv"))


# Create the day/night activity space table
DayNightTable <- FullTable[0,1:3]
DayNightTable <- lapply(mcp_day, calcMCPsize, level="day", export="table") %>% 
     bind_rows(DayNightTable)
DayNightTable <- lapply(mcp_night, calcMCPsize, level="night", export="table") %>% 
     bind_rows(DayNightTable)
DayNightTable <- DayNightTable %>%
     arrange(Transmitter, Level)
# Export day/night activity space table
write_excel_csv(DayNightTable, paste0(sinkPath, "MCP_DayNight_sizes.csv"))


##### Overlap Index of 50% MCPs #####
# To determine territoriality across the bay
# Make one dataset of the 50% full MCPs
mcp_50all <- bind_rows(mcp_50)
ggplot() + 
     geom_polygon(aes(x=x,y=y,color=transmitter), fill=NA, data = mcp_50all)
# mcp_95all <- bind_rows(mcp_95)
# ggplot() + 
#      geom_polygon(aes(x=x,y=y,color=transmitter), 
#                   fill=NA, data = mcp_95all) +
#      geom_polygon(aes(x=x,y=y,color=transmitter), linetype = "dashed",
#                   fill=NA, data = mcp_50all)
 
# Create the SpatialPolygons
sp50_all <- lapply(mcp_50, calcMCPsize, export="sp")
# Calculate the total area of overlap by adding
#     24797/59271
#     45334/45339
OverlapArea <- raster::area(raster::intersect(sp50_all[["A69-1601-24797"]],
                                              sp50_all[["A69-1601-59271"]])) + 
     raster::area(raster::intersect(sp50_all[["A69-1601-45334"]], 
                                    sp50_all[["A69-1601-45339"]]))
# Calculate the total area of all 50% MCPs
TotalArea50 <- raster::union(unlist(sp50_all)) %>% 
     raster::area() %>% # area of each listed polygon
     unlist() %>% # take them out of the list
     sum() # add them together
# Calculate the overlap index as area of overlap divided by total area
overlapI <- OverlapArea / (TotalArea50-OverlapArea) # don't double-count overlap
# Print the result
cat("\n50% MCP overlap index:", overlapI)

##### Day/Night activity space separation #####
# Calculate for each individual using day/night activity spaces
#    - overlap index (area of overlap / total area)
#    - centroid separation

# Calculate overlap index and centroid separation
# Use transmitter ID as input so can extract from two separate lists
DayNightSep <- function(trans){
     # Extract the points
     day <- mcp_day[[trans]]
     night <- mcp_night[[trans]]
     # Set parameters depending on projection
     proj <- ifelse(names(day)[1]=="x", TRUE, FALSE) # check if projected
     crs_proj <- ifelse(proj, # check projection
                        "+proj=utm +zone=20 +datum=NAD83 +units=m", # projected
                        "+proj=longlat +datum=NAD83") # lat/long
     coord_cols <- if(proj){ # check whether projection is wanted
          c("x","y") # projected
     } else {c("long", "lat")} # lat/long
     # Convert day and night to SpatialPolygons
     sp_day<-SpatialPolygons(list(Polygons(list(Polygon(coords=
                                    dplyr::select(day, coord_cols))),1)),
                         proj4string = CRS(crs_proj))
     sp_night<-SpatialPolygons(list(Polygons(list(Polygon(coords=
                                    dplyr::select(night, coord_cols))),1)),
                             proj4string = CRS(crs_proj))
     plot(sp_day)
     plot(sp_night, add=T)
     # Find the overlap index
     OI <- raster::intersect(sp_day,sp_night) %>% # take the intersection
          raster::area() %>% tibble(OverlapArea=.) %>% # find the overlap area
          mutate(TotalArea=raster::area(sp_day) + # area of day
                      raster::area(sp_night) - # plus area of night
                      OverlapArea) %>% # not duplicating the overlap
          mutate(OI=OverlapArea/TotalArea) %>% # calculate the OI
          dplyr::select(OI) %>% unlist() # extract the OI
     # Find the centroid separation
     diffx <- as.data.frame(gCentroid(sp_day))$x-as.data.frame(gCentroid(sp_night))$x
     diffy <- as.data.frame(gCentroid(sp_day))$y-as.data.frame(gCentroid(sp_night))$y
     sep <- sqrt(diffx^2+diffy^2) %>% round()
     # Create the table
     table <- tibble::tibble(Transmitter = trans,
                             OverlapI = OI,
                             CentroidSep = sep)
     return(table)
}
# Apply the function
DayNightSepTable <- lapply(names(mcp_day), DayNightSep) %>% bind_rows()

# Export day/night activity space table
write_excel_csv(DayNightSepTable, paste0(sinkPath, "MCP_DayNight_separation.csv"))