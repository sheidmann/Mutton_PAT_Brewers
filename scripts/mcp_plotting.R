# mcp_plotting.R

# Sarah Heidmann
# Created 22 Jan 2018
# Modified 2 Jun 2020

# Summary: Plots minimum convex polygons (MCPs) for Brewers Bay mutton snapper

# Data inputs:
#     - dataframes containing points depicting MCPs (exported from mcp_creation.R)
#     - dataframe containing points for Brewers Bay land polygon
# Actions:
#     - plots 95% and 50% MCPs for full data
# Data exports:
#     - facetted plot of 95% and 50% MCP for each fish

# Load the libraries
library(tidyverse)

##### Import the data #####
# Find the data
sourcePaths <- paste0("data/",c("mcp_95","mcp_50"),"/")
filenames <- list.files(sourcePaths[1])
# Read the data
readMCP <- function(filename, sourcePath){
     dat <- read_csv(paste0(sourcePath, filename))
     return(dat)
}
mcp_95 <- lapply(filenames, readMCP, sourcePaths[1]) %>% # read in the files
        bind_rows() # combine into one tibble
mcp_95

mcp_50 <- lapply(filenames, readMCP, sourcePaths[2]) %>% # read in the files
        bind_rows() # combin into one tibble
mcp_50

# Read in the land shapefile
landSTTSTJ <- read_csv("data/otherdata/STTSTJland.csv")

# Create the color list for plotting (colorblind friendly)
mnbColors <- c("#0072B2", #"#CC79A7",  # 36029 removed
               "#E69F00", #"#999999", # 45336 removed
               "#F0E442", "#009E73", "#56B4E9", "#D55E00")
mnbColors <- setNames(mnbColors, gsub(".csv","",filenames))

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
anchor_n <-data.frame(x=291000,y=2029900)
ht_n <- 150 # height of the arrow
wd_n <- 100 # width of the arrow
northarrow <- data.frame(x=c(anchor_n$x, anchor_n$x-(wd_n/2), # top, left bottom
                             anchor_n$x, anchor_n$x+(wd_n/2), # mid, right bottom
                             anchor_n$x), # top
                         y=c(anchor_n$y, anchor_n$y-ht_n, # top, left bottom
                             anchor_n$y-(ht_n*2/3),  # mid
                             anchor_n$y-ht_n, anchor_n$y)) # right bottom, top

# Make the plot
ggplot() +
        # Draw the land
        geom_polygon(data = landSTTSTJ, fill = "black",
                     aes(x_UTM20N, y_UTM20N, group = group)) +
        # Add the dashed 95% polygon (no points)
        geom_path(data = mcp_95, aes(x = x, y = y, color = transmitter), linetype = 2) +
        # Add the solid 50% polygon (no points)
        geom_path(data = mcp_50, aes(x = x, y = y, color = transmitter)) +
        # Add the scale bar
        geom_path(data = scalebar, aes(x=x,y=y), color = "black") +
        geom_text(data = anchor_s, aes(x=x+100,y=y-80), color = "black", 
                  label = "1 km", family = "Times New Roman", size = 6) +
        # Add the north arrow
        geom_polygon(data = northarrow, aes(x=x,y=y), fill = "white") +
        # Color formatting
        scale_color_manual("",values = mnbColors,
                           labels = gsub("A69-1601-","",names(mnbColors))) +
        guides(colour = guide_legend(override.aes = list(size=5))) +
        # Other formatting
        coord_equal(xlim = c(XMin,XMax),
                    ylim = c(YMin,YMax)) +
        xlab("") +
        ylab("") +
        theme(plot.margin=unit(c(0.1,0.1,0,0), "in"),
              panel.background = element_blank(),
              panel.border=element_rect(colour="black",size=1, fill = NA),
              legend.key = element_blank(),
              legend.position = "top",
              legend.text = element_text(family = "Times New Roman", size = 16),
              axis.text = element_blank(), axis.ticks = element_blank())
# Save the plot
ggsave(paste0(sinkPath,"MCP_allfacet.tiff"), width = 5.5, height = 9, units = "in")
