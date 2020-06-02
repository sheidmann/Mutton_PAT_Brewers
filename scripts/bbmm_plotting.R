#bbmm_plotting.R
# Sarah Heidmann

# Created 5 Jul 2017
# Last modified 1 Jun 2020

# Summary: Plots Brownian Bridge Movement Models (BBMMs) for Brewers Bay mutton snapper

# Data inputs:
#     - dataframes containing rasters depicting BBMMs (exported from bbmm_creation.R)
#     - shapefile of Brewers Bay land
# Actions:
#     - plots 95% BBMMs for full data
# Data exports:
#     - facetted plot of 95% BBMM for each fish

# Load the libraries
library(tidyverse)
library(RColorBrewer)

##### Import the data #####
# We're not going to use the tidyverse to import because these packages use data frames
sourcePath <- "data/bbmm_df/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames
importBBMM <- function(filename){
        # Read the file
        dat <- read_csv(paste0(sourcePath, filename))
        # Return the dataset
        return(dat)
}
mnb_bbmm <- lapply(filenames, importBBMM) %>% # read all the files into a list
        bind_rows() # combine into one big tibble for plotting
mnb_bbmm

# Read in the land shapefile
landSTTSTJ <- read_csv("data/otherdata/STTSTJland.csv")

# Set the location for the plot
sinkPath <- "outputs/"

##### Make the plot #####
# Fix the extent
XMin <- 289894
XMax <- 291283
YMin <- 2028438
YMax <- 2029931

# Construct the plot
ggplot(data=mnb_bbmm) +
        # Fill the raster
        geom_raster(aes(x=x,y=y,fill=ud)) +
        scale_fill_gradientn("Probability",colors=rev(brewer.pal(9, "RdYlBu"))) +
        # Plot the land
        geom_polygon(data = landSTTSTJdf, aes(x_UTM20N, y_UTM20N, group = group),
                     fill = "black") +
        # Set a consistent extent
        coord_equal(xlim = c(XMin,XMax),
                    ylim = c(YMin,YMax)) +
        # Facet by transmitter
        facet_wrap(~trans,ncol=2) +
        geom_text(size = 6, color = "white", # label the panels
                  aes(x=290600,y=2029850, family = "Times New Roman", 
                      label = substr(trans,10,14))) +
        # No axis labels
        xlab("")+
        ylab("")+
        # Formatting
        theme(strip.background = element_blank(), strip.text = element_blank(),
              # Plot formatting
              plot.margin=unit(c(0.1,0.1,0,0), "in"),
              panel.background = element_rect(fill=rev(brewer.pal(9,"RdYlBu"))[1]), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border=element_rect(colour="black",size=1, fill = NA),
              # Legend Formatting
              legend.position = "right", 
              legend.title = element_text(size=12),
              legend.text = element_text(size=10),
              legend.box.spacing = unit(c(0,0,0,0),"in"),
              legend.box.margin = margin(),
              # Text formatting
              axis.text = element_blank(), axis.ticks = element_blank(),
              text = element_text(family = "Times New Roman", size = 20))
# Save the plot
ggsave(paste0(sinkPath,"BBMM_allfacet.tiff"), width = 5.5, height = 9, units = "in")
