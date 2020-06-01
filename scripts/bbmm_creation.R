# bbmm_creation.R
# Sarah Heidmann
# Created 5 July 2017
# Last modified 1 Jun 2020

# Summary: Creates Brownian Bridge Movement Models (BBMMs) for Brewers Bay mutton snapper

# Data inputs:
#     - mutton snapper acoustic data: 4_binned_1hr (exported from 3_binning.R)
# Actions:
#     - creates 95% BBMMs for full data
# Data exports:
#     - raster of BBMM output

# Load the libraries
library(adehabitatLT)
library(adehabitatHR)
library(raster)
library(tidyverse)

##### Import the data #####
# We're not going to use the tidyverse to import because these packages use data frames
sourcePath <- "data/4_binned_1hr/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
importMNB <- function(filename){
     # Read the file
     dat <- read.csv(paste0(sourcePath, filename))
     # Remove time bins with no detections
     dat <- dat[dat$No.detections>0,]
     # Return the dataset
     return(dat)
}
mnb_ls <- lapply(filenames, importMNB) # read all the files into a list
names(mnb_ls) <- gsub(".csv", "", filenames) # take .csv out of the names
lapply(mnb_ls, head)

# Set the location for the table of BBMM sizes
sinkPath <- "outputs/"

##### Convert to ltraj #####
# https://cran.r-project.org/web/packages/adehabitatHR/adehabitatHR.pdf
# To use kernelbb() you need an object of class ltraj (adehabitatLT)
# Acoustic data is type II trajectory (has times of relocations)
makeltraj <- function(dataset){
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
mnb_lt <- lapply(mnb_ls, makeltraj)

head(mnb_lt[[1]][[1]]) # 24797
head(mnb_lt[[2]][[1]]) # 45334

##### Make the BBMMs #####
# Each will have different parameters, so need to do them separately.
# Some steps will need to be done manually.

# Parameters: 
# sig2 is supposed to be mean standard deviation of the relocations
# sig1 from maximum likelihood estimation: adehabitatHR::liker()
# If sig1=0, decrease sig2 until sig1 is nonzero

# Write the function to make the BBMM
makeBBMM <- function(dat_lt, sig2_est, sig1_est,
                     dfPath="", rasterPath=""){
        # Create it based on estimated parameters
        bbmm <- kernelbb(dat_lt, sig1 = sig1_est, sig2 = sig2_est, grid = 50)
        # Some ways to look at it
        # image(bbmm)
        # plot(bbmm)
        # plot(getverticeshr(bbmm, 95), add=TRUE, lwd=2)
        
        # Make the size table
        # Extract the transmitter
        trans <- attr(dat_lt[[1]],"id")
        # Set the level of interest
        level = 95
        # Calculate the area
        area <- adehabitatHR::kernel.area(bbmm, percent = level, unout = "m2")
        # Create the table
        table <- tibble::tibble(Transmitter = trans,
                                Level = level,
                                Size.m2 = area)
        # If raster export is specified:
        if(rasterPath!=""){
                # Make the raster
                bbmm_r <- raster(as(bbmm,"SpatialPixelsDataFrame"))
                # Export the raster
                writeRaster(bbmm_r, options=c('TFW=YES'),
                            filename = paste0(rasterPath,trans,".tif"))
        }
        # If dataframe export is specified (for future plotting):
        if(dfPath!=""){
                # Convert the estUD to a raster
                bbmm_r <- raster(as(bbmm,"SpatialPixelsDataFrame"))
                # Convert the raster to points
                bbmm_p <- rasterToPoints(bbmm_r)
                # Make the points a dataframe for ggplot
                bbmm_df <- data.frame(bbmm_p)
                # Set the transmitter id
                bbmm_df$trans <- trans
                # Export
                write.csv(bbmm_df, paste0(dfPath,trans,".csv"), row.names = F)
        }
        
        return(table)
}

# Create the full home range table
FullTable <- tibble::tibble(Transmitter = character(),
                            Level = character(),
                            Size.m2 = double())

# 24797
# find sig2
sd(mnb_lt[["A69-1601-24797"]][[1]]$dist, na.rm = TRUE)
# sig2=76 but this gives me sig1=0, so decrease until nonzero (sig2=72)
# Try maximum likelihood estimation to get sig1
liker(mnb_lt[["A69-1601-24797"]], sig2 = 72, rangesig1 = c(0, 1))[[1]]$sig1
# sig1= 0.05
# Do the BBMM
FullTable <- makeBBMM(mnb_lt[["A69-1601-24797"]], sig2_est = 72, sig1_est = 0.05,
                      dfPath = "outputs/bbmm_df/") %>%
        bind_rows(FullTable)

# 45334
# find sig2
sd(mnb_lt[["A69-1601-45334"]][[1]]$dist, na.rm = TRUE)
# sig2=80 but this gives me sig1=0, so decrease until nonzero (sig2=62)
# Try maximum likelihood estimation to get sig1
liker(mnb_lt[["A69-1601-45334"]], sig2 = 62, rangesig1 = c(0, 1))[[1]]$sig1
# sig1= 0.12
# Do the BBMM
FullTable <- makeBBMM(mnb_lt[["A69-1601-45334"]], sig2_est = 62, sig1_est = 0.12,
                      dfPath = "outputs/bbmm_df/") %>%
        bind_rows(FullTable)

# 45337
# find sig2
sd(mnb_lt[["A69-1601-45337"]][[1]]$dist, na.rm = TRUE)
# sig2=91 but this gives me sig1=0, so decrease until nonzero (sig2=78)
# Try maximum likelihood estimation to get sig1
liker(mnb_lt[["A69-1601-45337"]], sig2 = 78, rangesig1 = c(0, 1))[[1]]$sig1
# sig1= 0.12
# Do the BBMM
FullTable <- makeBBMM(mnb_lt[["A69-1601-45337"]], sig2_est = 78, sig1_est = 0.12,
                      dfPath = "outputs/bbmm_df/") %>%
        bind_rows(FullTable)

# 45338
# find sig2
sd(mnb_lt[["A69-1601-45338"]][[1]]$dist, na.rm = TRUE)
# sig2=59 but this gives me sig1=0, so decrease until nonzero (sig2=52)
# Try maximum likelihood estimation to get sig1
liker(mnb_lt[["A69-1601-45338"]], sig2 = 52, rangesig1 = c(0, 1))[[1]]$sig1
# sig1= 0.11
# Do the BBMM
FullTable <- makeBBMM(mnb_lt[["A69-1601-45338"]], sig2_est = 52, sig1_est = 0.11,
                      dfPath = "outputs/bbmm_df/") %>%
        bind_rows(FullTable)

# 45339
# find sig2
sd(mnb_lt[["A69-1601-45339"]][[1]]$dist, na.rm = TRUE)
# sig2=43
# Try maximum likelihood estimation to get sig1
liker(mnb_lt[["A69-1601-45339"]], sig2 = 43, rangesig1 = c(0, 1))[[1]]$sig1
# sig1= 0.34
# Do the BBMM
FullTable <- makeBBMM(mnb_lt[["A69-1601-45339"]], sig2_est = 43, sig1_est = 0.34,
                      dfPath = "outputs/bbmm_df/") %>%
        bind_rows(FullTable)

# 59271
# find sig2
sd(mnb_lt[["A69-1601-59271"]][[1]]$dist, na.rm = TRUE)
# sig2=55
# Try maximum likelihood estimation to get sig1
liker(mnb_lt[["A69-1601-59271"]], sig2 = 55, rangesig1 = c(0, 1))[[1]]$sig1
# sig1= 0.74
# Do the BBMM
FullTable <- makeBBMM(mnb_lt[["A69-1601-59271"]], sig2_est = 55, sig1_est = 0.74,
                      dfPath = "outputs/bbmm_df/") %>%
        bind_rows(FullTable)

# Format the BBMM home range table
FullTable <- FullTable %>% 
        arrange(Transmitter, desc(Level))  %>% # Sort the output
        mutate(Size.km2 = Size.m2 / 1000000) # Convert to km2
# Export full home range table
# write_excel_csv(FullTable, paste0(sinkPath, "BBMM_Full_sizes.csv"))

