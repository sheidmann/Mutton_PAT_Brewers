# mcp_habitats.R

# Sarah Heidmann
# Created 27 Jan 2018
# Last modified 6 Jun 2020

# Summary: calculates habitat composition of MCPs for Brewers Bay mutton snapper

# Data inputs:
#     - data frames of MCP polygon vertices
#         - 95% full, 50% full, 95% day, 95% night
# Actions:
#     - calculates percent coverage of each habitat type of each MCP
#     - calculates reef:seagrass ratio for each MCP
#     - calculates summary statistics and t-tests
# Data exports:
#     - table of full 95% and 50% MCP habitat composition
#     - table of each fish's day/night habitat composition

# Load the libraries
library(tidyverse)
library(sp)
library(raster)
library(rgdal)

##### Import the data #####
# Find the MCP point data
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

# Import the habitat shapefile
mnb_bh <- readOGR(dsn = "/Volumes/Squishy2/2016_MareNostrum_BenthicHabitat/01_GISData/BHabitat_PBRBay_STT_1-1K", 
                  layer = "2016_BHabitat_BPBay_STT_1-1K_NAD83_UTM_20N_V6")
plot(mnb_bh)
crs(mnb_bh)

# Set the output location
sinkPath <- "outputs/"

##### Find habitat composition #####
# The types in the raw benthic habitat map is a bit confusing.
# We use M_STRUCT to see if coral reef, and M_COVER to see if seagrass.
# All other types will be "other"
# Add my own column to make it easy.
mnb_bh$mytype <- case_when(mnb_bh$M_STRUCT=="Coral Reef and Hardbottom"~"CoralReef",
                           mnb_bh$M_COVER=="Seagrass"~"Seagrass",
                           TRUE~"Other")
# Create a function to convert to SpatialPolygons and calculate habitat composition
habComp <- function(dataset, level, export){
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
     # Project to match benthic
     sp <- spTransform(sp, CRS(as.character(crs(mnb_bh))))
     # Find the total area of the MCP
     totalArea <- raster::area(sp)
     # Find the intersection
     habs <- raster::intersect(sp, mnb_bh)
     # Make the table of habitat percent cover
     table <- tibble::tibble(Transmitter=trans,
                             Level = level, 
                             habtype = habs$mytype, # set the types
                             Area.m2=raster::area(habs)) %>% # set the areas
             group_by(Transmitter,Level,habtype) %>% 
             summarise(Area.m2 = sum(Area.m2)) %>% # sum duplicate type polygons
             ungroup() %>%
             mutate(percent = round(Area.m2 / totalArea*100,1)) %>% # calc percent cover
             pivot_wider(id_cols=c(Transmitter,Level), # give hab types own columns
                         names_from = habtype, values_from = percent) %>%
             add_column(!!!c("CoralReef"=0, # if add missing columns for ratio
                             "Seagrass"=0)[!c("CoralReef",
                                              "Seagrass") %in% names(.)]) %>%
     mutate(ReefSeagrassRatio=round(log10(round(CoralReef)/
                                            round(Seagrass)),1)) # calculate ratio
     # Export either the table of sizes or the SpatialPolygons object
     if(export=="table"){
          return(table)
     } else if(export=="sp"){return(habs)}
     
}

# Create the full home range table
FullHabTable <- tibble::tibble(Transmitter = character(), # create empty table
                            Level = character(),
                            CoralReef = double(),
                            Seagrass = double(),
                            Other = double(),
                            ReefSeagrassRatio=double())
FullHabTable <- lapply(mcp_95 ,habComp, level="95", export="table") %>% 
        bind_rows(FullHabTable) # add 95% MCPs
FullHabTable <- lapply(mcp_50, habComp, level="50", export="table") %>% 
        bind_rows(FullHabTable) # add 50% MCPs
FullHabTable <- FullHabTable %>% 
        replace(is.na(.),0) %>% # change NAs to 0 coverage
        arrange(Transmitter, desc(Level)) %>% # sort the rows
        dplyr::select(Transmitter,Level, # sort the columns
                      CoralReef,Seagrass,Other,ReefSeagrassRatio)

# Export full home range table
# write_excel_csv(FullHabTable, paste0(sinkPath, "MCP_Full_habitats.csv"))

# Create the day/night space table
DayNightHabTable <- FullHabTable[0,1:6] # create empty table
DayNightHabTable <- lapply(mcp_day ,habComp, level="day", export="table") %>% 
        bind_rows(DayNightHabTable) # add day MCPs
DayNightHabTable <- lapply(mcp_night, habComp, level="night", export="table") %>% 
        bind_rows(DayNightHabTable) # add night MCPs
DayNightHabTable <- DayNightHabTable %>% 
        replace(is.na(.),0) %>% # Change NAs to 0 coverage
        arrange(Transmitter, Level) %>%# Sort the rows
        dplyr::select(Transmitter,Level, # sort the columns
                      CoralReef,Seagrass,Other,ReefSeagrassRatio)
# Export full home range table
# write_excel_csv(DayNightHabTable, paste0(sinkPath, "MCP_DayNight_habitats.csv"))

##### Habitat summary statistics #####
# Full home ranges (95% MCPs)
# Mean reef and seagrass coverage
FullHabTable %>%
        filter(Level=="95") %>%
        summarise(meanreef = mean(CoralReef),
                  meanseagrass=mean(Seagrass),
                  sereef = sd(CoralReef)/sqrt(length(CoralReef)),
                  meanratio=mean(ReefSeagrassRatio),
                  seratio=sd(ReefSeagrassRatio)/sqrt(length(ReefSeagrassRatio)))

# Compare coralreef:seagrass ratio to an equal ratio of 0
# Since small sample size, need to check for normality with Shapiro-Wilks
# null: data is normal; alternative: data not normal
# First remove the infinite value
ratios <- filter(FullHabTable,Level =="95")$ReefSeagrassRatio
shapiro.test(ratios) # p = 0.29 (normal)
# Test hypothesis that ratio != 0
t.test(ratios, mu=0)
# significant at p <0.01

# Day and night activity spaces (95% day/night MCPs)
# Day summary
DayNightHabTable %>%
        filter(Level=="day") %>%
        summarise(meanreef = mean(CoralReef),
                  meanseagrass=mean(Seagrass),
                  sereef = sd(CoralReef)/sqrt(length(CoralReef)),
                  meanratio=mean(ReefSeagrassRatio),
                  seratio=sd(ReefSeagrassRatio)/sqrt(length(ReefSeagrassRatio)))
# Night summary
DayNightHabTable[8,6] <- -2 # Replace the infinite value
DayNightHabTable %>%
        filter(Level=="night") %>%
        summarise(meanreef = mean(CoralReef),
                  meanseagrass=mean(Seagrass),
                  sereef = sd(CoralReef)/sqrt(length(CoralReef)),
                  meanratio=mean(ReefSeagrassRatio),
                  seratio=sd(ReefSeagrassRatio)/sqrt(length(ReefSeagrassRatio)))

# Paired t-test of ratio between day and night
dayratio <- DayNightHabTable %>% filter(Level =="day") %>% .$ReefSeagrassRatio
nightratio <- DayNightHabTable %>% filter(Level =="night") %>% .$ReefSeagrassRatio
# Since small sample size, need to check for normality with Shapiro-Wilks
# null: data is normal; alternative: data not normal
shapiro.test(dayratio) # p = 0.25 (normal)
shapiro.test(nightratio) # p = 0.67 (normal)
# Test for equal variance
var.test(dayratio,nightratio)
# No evidence that variances are not equal (p=0.49)
# Test hypothesis that night != day
t.test(dayratio,nightratio, paired = TRUE, var.equal = TRUE)
# No evidence to suggest they are different (p=0.12)