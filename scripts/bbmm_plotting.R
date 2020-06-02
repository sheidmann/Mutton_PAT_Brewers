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

# Set parameters for scale bar
anchor_s <- data.frame(x=289860,y=2028750) # Anchor point (center left)
ht_s <- 25 # height of the ticks (total height is double)
scalebar <- data.frame(x=c(anchor_s$x,anchor_s$x,anchor_s$x,# left tick
                           anchor_s$x+1000, # line
                           anchor_s$x+1000,anchor_s$x+1000), # right tick
                       y=c(anchor_s$y+ht_s,anchor_s$y-ht_s,anchor_s$y,# left tick
                           anchor_s$y, # line
                           anchor_s$y+ht_s,anchor_s$y-ht_s))  # right tick
# Set parameters for North arrow
anchor_n <-data.frame(x=291200,y=2029900)
ht_n <- 150 # height of the arrow
wd_n <- 100 # width of the arrow
northarrow <- data.frame(x=c(anchor_n$x, anchor_n$x-(wd_n/2), # top, left bottom
                             anchor_n$x, anchor_n$x+(wd_n/2), # mid, right bottom
                             anchor_n$x), # top
                         y=c(anchor_n$y, anchor_n$y-ht_n, # top, left bottom
                             anchor_n$y-(ht_n*2/3),  # mid
                             anchor_n$y-ht_n, anchor_n$y)) # right bottom, top

# Construct the plot
ggplot(data=mnb_bbmm) +
        # Fill the raster
        geom_raster(aes(x=x,y=y,fill=ud)) +
        scale_fill_gradientn("Probability",colors=rev(brewer.pal(9, "RdYlBu"))) +
        # Plot the land
        geom_polygon(data = landSTTSTJ, aes(x_UTM20N, y_UTM20N, group = group),
                     fill = "black") +
        # Set a consistent extent
        coord_equal(xlim = c(XMin,XMax),
                    ylim = c(YMin,YMax)) +
        # Facet by transmitter
        facet_wrap(~trans,ncol=2) +
        geom_text(size = 6, color = "white", # label the panels
                  aes(x=290600,y=2029850, family = "Times New Roman", 
                      label = substr(trans,10,14))) +
        # Add the scale bar
        geom_path(data = scalebar, aes(x=x,y=y), color = "black") +
        geom_text(data = anchor_s, aes(x=x+150,y=y-80), color = "black", 
                  label = "1 km", family = "Times New Roman", size = 4) +
        # Add the north arrow
        geom_polygon(data = northarrow, aes(x=x,y=y), fill = "white") +
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
