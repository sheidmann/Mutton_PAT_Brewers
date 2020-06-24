# mcp_habitats.R

# Sarah Heidmann
# Created 27 Jan 2018
# Last modified 24 Jun 2020

# Summary: calculates habitat composition of MCPs for Brewers Bay mutton snapper

# Data inputs:
#     - data frames of MCP polygon vertices
#         - 95% full, 50% full, 95% day, 95% night
#     - shapefile of Brewers Bay habitats
# Actions:
#     - calculates percent coverage of each habitat type of each MCP
#     - calculates habitat selection index
#     - calculates summary statistics and t-tests
# Data exports:
#     - table of full 95% and 50% MCP habitat composition
#     - table of each fish's day/night habitat composition
#     - table of full 95% MCP habitat selection indices

# Load the libraries
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)

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
#plot(mnb_bh)
crs(mnb_bh)
# deletes space at end (problems with intersect)
mnb_bh@proj4string <- crs(as.character(crs(mnb_bh)))

# Set the output location
sinkPath <- "outputs/"

##### Explore habitats #####
# The types in the raw benthic habitat map are a bit confusing.
# Let's look at all the options present.
habitats <- as_tibble(mnb_bh[c("ZONE","M_STRUCT","D_STRUCT","P_HARD","M_COVER","P_COVER","COVER","P_CORAL")]) %>%
        unique() %>%
        arrange(M_STRUCT,D_STRUCT,M_COVER)
habitats
# We use D_STRUCT to see if coral reef, and M_COVER to see if seagrass.
# All other types will be "other"
# Add my own column to make it easy.
reeftypes <- c("Aggregate Reef","Aggregated Patch Reefs","Individual Patch Reef")
pavtypes <- c("Pavement","Pavement with Sand Channels")
mnb_bh$mytype <- case_when(mnb_bh$D_STRUCT %in% reeftypes ~ "CoralReef",
                           mnb_bh$M_COVER=="Seagrass" ~ "Seagrass",
                           mnb_bh$D_STRUCT %in% pavtypes ~ "Pavement",
                           TRUE ~ "Other")

##### Calculate available habitats #####
# Find the create a single polygon for the extent of the array
# Made from receiver detection ranges
statmaster <- read_csv("data/otherdata/mnb_station_master_2017.csv")
# Create the receiver detection ranges
buffers <- gBuffer(SpatialPoints(coords = cbind(statmaster$x_UTM20N,
                                                statmaster$y_UTM20N),
                                 proj4string = mnb_bh@proj4string),
                      byid= TRUE, width = statmaster$V13Buffer70)
#plot(buffers)
# Merge them together
buffer <- unionSpatialPolygons(buffers, IDs = rep(1,43))
# What is the total array area? 
arrayArea <- raster::area(buffer) # 1.5 km2
# Cut the habitats to the array area
arrayHabs <- raster::intersect(buffer, mnb_bh)
# What are the total habitat areas?
arrayHabTab <- tibble(habtype = arrayHabs$mytype, # set the types
                      Area.m2=raster::area(arrayHabs)) %>% # find the areas
        group_by(habtype) %>%  # for each habitat type
        summarise(Area.m2 = sum(Area.m2), # sum duplicate type polygons
                  .groups="drop") %>% # remove the grouping
        mutate(TotProp=round((Area.m2 / arrayArea*100),1)) %>% # habitat proportions
        dplyr::select(habtype,TotProp) %>%
        pivot_wider(names_from = habtype,values_from = TotProp)
arrayHabTab
# Proportions only add up to 94%.
# This is because detection ranges extend beyond habitat map.
# The remaining 6% is "unknown"

##### Find MCP habitat composition #####
# Create a function to convert to SpatialPolygons and calculate habitat composition
habComp <- function(dataset, level="", export){
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
             summarise(Area.m2 = sum(Area.m2), # sum duplicate type polygons
                       .groups="drop") %>% # alternative to ungroup()
             mutate(percent = round(Area.m2 / totalArea*100,1)) %>% # calc percent cover
             pivot_wider(id_cols=c(Transmitter,Level), # give hab types own columns
                         names_from = habtype, values_from = percent) %>%
             add_column(!!!c("Seagrass"=0, # if add missing columns for ratio
                             "CoralReef"=0,
                             "Pavement"=0,
                             "Other"=0)[!c("Seagrass","CoralReef","Pavement",
                                           "Other") %in% names(.)]) %>%
             relocate(c(Seagrass,CoralReef,Pavement,Other), .after=Level)
     # Export either the table of sizes or the SpatialPolygons object
     if(export=="table"){
          return(table)
     } else if(export=="sp"){return(habs)}
     
}

# Create the full home range table
FullHabTable <- tibble::tibble(Transmitter = character(), # create empty table
                            Level = character(),
                            Seagrass = double(),
                            CoralReef = double(),
                            Pavement = double(),
                            Other = double())
FullHabTable <- lapply(mcp_95 ,habComp, level="95", export="table") %>% 
        unname() %>%
        bind_rows(FullHabTable) # add 95% MCPs
FullHabTable <- lapply(mcp_50, habComp, level="50", export="table") %>% 
        unname() %>%
        bind_rows(FullHabTable) # add 50% MCPs
FullHabTable <- FullHabTable %>% 
        replace(is.na(.),0) %>% # change NAs to 0 coverage
        arrange(Transmitter, desc(Level)) %>% # sort the rows
        dplyr::select(Transmitter,Level, # sort the columns
                      Seagrass,CoralReef,Pavement,Other)
# Look at it
FullHabTable

# Export full home range table
# write_excel_csv(FullHabTable, paste0(sinkPath, "MCP_Full_habitats.csv"))

# Create the day/night space table
DayNightHabTable <- FullHabTable[0,1:6] # create empty table
DayNightHabTable <- lapply(mcp_day ,habComp, level="day", export="table") %>%
        unname() %>%
        bind_rows(DayNightHabTable) # add day MCPs
DayNightHabTable <- lapply(mcp_night, habComp, level="night", export="table") %>% 
        unname() %>%
        bind_rows(DayNightHabTable) # add night MCPs
DayNightHabTable <- DayNightHabTable %>% 
        replace(is.na(.),0) %>% # Change NAs to 0 coverage
        arrange(Transmitter, Level) %>%# Sort the rows
        dplyr::select(Transmitter,Level, # sort the columns
                      Seagrass,CoralReef,Pavement,Other)
# Look at it
DayNightHabTable

# Export full home range table
# write_excel_csv(DayNightHabTable, paste0(sinkPath, "MCP_DayNight_habitats.csv"))

##### Habitat Selection Index (HSI) #####
# Calculated by dividing the proportion of habitat used by the proportion available
# Such that degree of preference or avoidance for a habitat is shown
#       by the degree of deviation from 1 (positive or negative, respectively)
FullHSItable <- FullHabTable %>%
        filter(Level=="95") %>% # only look at full home ranges
        dplyr::select(-Level) %>% # reduce dataset
        # Calculate HSI for each habitat
        mutate(Seagrass=round(Seagrass/arrayHabTab$Seagrass,1),
               CoralReef=round(CoralReef/arrayHabTab$CoralReef,1),
               Pavement=round(Pavement/arrayHabTab$Pavement,1),
               Other=round(Other/arrayHabTab$Other,1))

# Export full home range HSI table
# write_excel_csv(FullHSItable, paste0(sinkPath, "MCP_Full_HSI.csv"))

##### Habitat summary statistics #####
# Full home ranges (95% MCPs)
# Mean habitat coverage
FullHabTable %>%
        filter(Level=="95") %>%
        summarise(meanseagrass=mean(Seagrass),
                  meanreef = mean(CoralReef),
                  meanpave = mean(Pavement),
                  meanother = mean(Other))
# Mean HSI
FullHSItable %>%
        summarise(meanseagrass=mean(Seagrass),
                  meanreef = mean(CoralReef),
                  meanpave = mean(Pavement),
                  meanother = mean(Other))
        

# Compare HSI to an equal ratio of 1 with Mann-Whitney-Wilcoxon test
wilcox.test(FullHSItable$Seagrass, mu=1)# significant at p=0.03
# Again for reef
wilcox.test(FullHSItable$CoralReef, mu=1) # not significant at p=0.8
# Again for pavement
wilcox.test(FullHSItable$Pavement, mu=1) # not significant at p=0.83
# Can't test "other" since all values identical and 0


# Day and night activity spaces (95% day/night MCPs)
# Day summary
DayNightHabTable %>%
        filter(Level=="day") %>%
        summarise(meanseagrass=mean(Seagrass),
                  meanreef = mean(CoralReef),
                  meanpave=mean(Pavement),
                  meanother=mean(Other))
# Night summary
DayNightHabTable %>%
        filter(Level=="night") %>%
        summarise(meanseagrass=mean(Seagrass),
                  meanreef = mean(CoralReef),
                  meanpave=mean(Pavement),
                  meanother=mean(Other))

# Paired Mann-Whitney-Wilcoxon test of proportions between day and night
# start with seagrass
dayseagrass <- DayNightHabTable %>% filter(Level =="day") %>% .$Seagrass
nightseagrass <- DayNightHabTable %>% filter(Level =="night") %>% .$Seagrass
# Test for equal variance
var.test(dayseagrass,nightseagrass) # No evidence variances not equal (p=0.72)
# Test hypothesis that night != day
wilcox.test(dayseagrass, nightseagrass, paired=TRUE)
# Suggestive evidence they are different (p=0.06)

# Now reef
dayreef <- DayNightHabTable %>% filter(Level =="day") %>% .$CoralReef
nightreef <- DayNightHabTable %>% filter(Level =="night") %>% .$CoralReef
var.test(dayreef,nightreef)# No evidence that variances are not equal (p=0.19)
# Test hypothesis that night != day
wilcox.test(dayreef,nightreef, paired = TRUE)
# No evidence they are different (p=0.2)

# Now pavement
daypave <- DayNightHabTable %>% filter(Level =="day") %>% .$Pavement
nightpave <- DayNightHabTable %>% filter(Level =="night") %>% .$Pavement
var.test(daypave,nightpave)# No evidence that variances are not equal (p=0.72)
# Test hypothesis that night != day
wilcox.test(daypave,nightpave, paired = TRUE)
# No evidence they are different (p=0.6)



