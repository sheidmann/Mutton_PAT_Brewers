# mcp_plotting.R

# Sarah Heidmann
# Created 22 Jan 2018
# Modified 3 Jun 2020

# Summary: Plots minimum convex polygons (MCPs) for Brewers Bay mutton snapper

# Data inputs:
#     - dataframes containing points depicting MCPs (exported from mcp_creation.R)
#     - dataframe containing points for Brewers Bay land polygon
# Actions:
#     - plots 95% and 50% MCPs for full data
#     - plots 95% MCPs for day and night data
# Data exports:
#     - facetted plot of 95% and 50% MCP for each fish
#     - facetted plot of 95% day/night MCPs for each fish

# Load the libraries
library(tidyverse)

##### Import the data #####
# Find the data
sourcePaths <- paste0("data/",c("mcp_95","mcp_50","mcp_day","mcp_night"),"/")
filenames <- list.files(sourcePaths[1])
# Read the data
readMCP <- function(filename, sourcePath){
     dat <- read_csv(paste0(sourcePath, filename))
     return(dat)
}
# Read 95% full MCPs
mcp_95 <- lapply(filenames, readMCP, sourcePaths[1]) %>% # read in the files
        bind_rows() %>% # combine into one tibble
        add_column(type = "95") # add type column for plotting
mcp_95

# Read 50% full MCPs
mcp_50 <- lapply(filenames, readMCP, sourcePaths[2]) %>% # read in the files
        bind_rows() %>% # combine into one tibble
        add_column(type="50")
mcp_50

# Read 95% day MCPs
mcp_day <- lapply(filenames, readMCP, sourcePaths[3]) %>% # read in the files
        bind_rows() %>% # combine into one tibble
        add_column(type = "day") # add type column for plotting
mcp_day

# Read 95% night MCPs
mcp_night <- lapply(filenames, readMCP, sourcePaths[4]) %>% # read in the files
        bind_rows() %>% # combine into one tibble
        add_column(type = "night") # add type column for plotting
mcp_night

# Read in the land shapefile
landSTTSTJ <- read_csv("data/otherdata/STTSTJland.csv")

# Read in the catch locations
transmaster <- read_csv("data/otherdata/mnb_mutton_transmitter_master_2017.csv") %>%
        filter(transmitter %in% gsub(".csv","",filenames))

# Set the location for the plot
sinkPath <- "outputs/"

##### Set plot parameters #####
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

##### Make the full MCP plot #####
# Make the plot
ggplot() +
        # Draw the land
        geom_polygon(data = landSTTSTJ, fill = "black",
                     aes(x_UTM20N, y_UTM20N, group = group)) +
        # Add the dashed 95% polygon
        geom_path(data = mcp_95, aes(x = x, y = y, linetype = type)) +
        # Add the solid 50% polygon
        geom_path(data = mcp_50, aes(x = x, y = y, linetype = type)) +
        # Add the catch location
        geom_point(data = transmaster, size = 3,
                   aes(x=release_x,y=release_y, shape=Project_objective)) +
        # Facet by transmitter
        facet_wrap(~transmitter, ncol = 2) +
        geom_text(size = 6, color = "white", data = mcp_95, # label the panels
                  aes(x=290600,y=2029850, family = "Times New Roman", 
                      label = substr(transmitter,10,14))) +
        # Add the scale bar
        geom_path(data = scalebar, aes(x=x,y=y), color = "black") +
        geom_text(data = anchor_s, aes(x=x+150,y=y-80), color = "black", 
                  label = "1 km", family = "Times New Roman", size = 4) +
        # Add the north arrow
        geom_polygon(data = northarrow, aes(x=x,y=y), fill = "white") +
        # Legend setup
        scale_linetype_manual("", values=c("95"="dashed","50"="solid"),
                              labels=c("95"="95% MCP","50"="50% MCP"))+
        scale_shape_manual("", values=c("Mare Nostrum Brewers"=24),labels=c("Catch Location")) +
        # Other formatting
        coord_equal(xlim = c(XMin,XMax),
                    ylim = c(YMin,YMax)) +
        xlab("") +
        ylab("") +
        theme(plot.margin=unit(c(0.1,0.1,0,0), "in"),
              # Panel formatting
              panel.background = element_blank(),
              panel.border=element_rect(colour="black",size=1, fill = NA),
              strip.background = element_blank(), strip.text = element_blank(),
              # Legend formatting
              legend.position = "bottom",
              legend.key = element_blank(),
              legend.box="vertical",
              legend.margin = margin(-0.2,0,0,0, unit="cm"),
              legend.text = element_text(family = "Times New Roman", size=12,
                                         margin = margin(r=1, l=0.2, unit = 'cm')),
              # Text formatting
              axis.text = element_blank(), axis.ticks = element_blank())
# Save the plot
ggsave(paste0(sinkPath,"MCP_bw_allfacet.tiff"), width = 5.5, height = 9, units = "in")


##### Make the day/night MCP plot #####
# Make the plot
ggplot() +
        # Draw the land
        geom_polygon(data = landSTTSTJ, fill = "black",
                     aes(x_UTM20N, y_UTM20N, group = group)) +
        # Add the day polygon
        geom_path(data = mcp_day, aes(x = x, y = y, linetype = type)) +
        # Add the night polygon
        geom_path(data = mcp_night, aes(x = x, y = y, linetype = type)) +
        # Facet by transmitter
        facet_wrap(~transmitter, ncol = 2) +
        geom_text(size = 6, color = "white", data = mcp_day, # label the panels
                  aes(x=290600,y=2029850, family = "Times New Roman", 
                      label = substr(transmitter,10,14))) +
        # Add the scale bar
        geom_path(data = scalebar, aes(x=x,y=y), color = "black") +
        geom_text(data = anchor_s, aes(x=x+150,y=y-80), color = "black", 
                  label = "1 km", family = "Times New Roman", size = 4) +
        # Add the north arrow
        geom_polygon(data = northarrow, aes(x=x,y=y), fill = "white") +
        # Legend setup
        scale_linetype_manual("", values=c("day"="dotted","night"="dotdash"),
                              labels = c("day"="Daytime 95% MCP", 
                                         "night"="Nighttime 95% MCP"))+
        # Other formatting
        coord_equal(xlim = c(XMin,XMax),
                    ylim = c(YMin,YMax)) +
        xlab("") +
        ylab("") +
        theme(plot.margin=unit(c(0.1,0.1,0,0), "in"),
              # Panel formatting
              panel.background = element_blank(),
              panel.border=element_rect(colour="black",size=1, fill = NA),
              strip.background = element_blank(), strip.text = element_blank(),
              # Legend formatting
              legend.position = "bottom",
              legend.key = element_blank(),
              legend.text = element_text(family = "Times New Roman", size=12,
                                         margin = margin(r=1, l=0.2, unit = 'cm')),
              # Text formatting
              axis.text = element_blank(), axis.ticks = element_blank())
# Save the plot
ggsave(paste0(sinkPath,"MCP_daynight_bw_allfacet.tiff"), width = 5.5, height = 9, units = "in")
