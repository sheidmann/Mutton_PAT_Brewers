# habitat_plotting.R

# Sarah Heidmann
# Created 27 Jan 2018
# Last modified 24 Jun 2020

# Summary: plots habitats of Brewers Bay for visualization of MCPs
# NOTE: these plots are cursory, not intended for publication.

# Data inputs:
#     - shapefile of Brewers Bay habitats
#     - data frames of MCP polygon vertices
#         - 95% full, 50% full, 95% day, 95% night
# Actions:
#     - visually explores Brewers Bay habitat polygons
#         - with and without MCPs
# Data exports:
#     - plot of 95% MCPs over benthic habitat map

# Load the libraries
library(rgdal)
library(raster)
library(tidyverse)

# Set output plot location
sinkPath <- "outputs/"

##### Import and format the benthic data #####
mnb_bh <- readOGR(dsn = "/Volumes/Squishy2/2016_MareNostrum_BenthicHabitat/01_GISData/BHabitat_PBRBay_STT_1-1K", 
                  layer = "2016_BHabitat_BPBay_STT_1-1K_NAD83_UTM_20N_V6")
# Plot and check projection
plot(mnb_bh)
crs(mnb_bh)

# Format to data frame for plotting
mnb_bh@data$id <- rownames(mnb_bh@data)
mnb_bh_pts <- fortify(mnb_bh, region = "id")
names(mnb_bh_pts) <- c("x","y","order","hole","piece","id","group")
mnb_bh_df <- plyr::join(mnb_bh_pts, mnb_bh@data, by = "id")

# Check it
ggplot(data = mnb_bh_df, aes(x, y, group = group)) +
     #geom_polygon(color = "black", fill = "black") + # makes it all black
     geom_path(color = "black")+
     coord_equal()

##### Explore habitat types #####
# We use D_STRUCT to see if coral reef, and M_COVER to see if seagrass.
# D_STRUCT also tells us land, pavement, and artificial
# All other types will be "other"
# Add my own column to make it easy.
reeftypes <- c("Aggregate Reef","Aggregated Patch Reefs","Individual Patch Reef")
pavtypes <- c("Pavement","Pavement with Sand Channels")
mnb_bh_df$mytype <- case_when(mnb_bh_df$D_STRUCT %in% reeftypes ~ "CoralReef",
                           mnb_bh_df$M_COVER=="Seagrass" ~ "Seagrass",
                           mnb_bh_df$D_STRUCT=="Land" ~ "Land",
                           mnb_bh_df$D_STRUCT %in% pavtypes ~ "Pavement",
                           mnb_bh_df$D_STRUCT =="Artificial" ~ "Artificial",
                           TRUE ~ "Other")
# Check out the types
summary(as.factor(mnb_bh_df$mytype))
summary(mnb_bh_df[mnb_bh_df$mytype=="Other",]$D_STRUCT)

# Plot the various categories
ggplot(data = mnb_bh_df, aes(x, y, 
                             fill=mytype,
                             #fill=D_STRUCT,
                             #fill=M_COVER,
                             #fill = P_COVER, 
                             group = group)) +
     geom_polygon()+
     coord_equal()


##### Add the MCPs #####
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
mcp_95 <- bind_rows(mcp_95)

# Plot MCPs over the habitat map
# Fix the extent
XMin <- 289894
XMax <- 291283
YMin <- 2028438
YMax <- 2029931
ggplot(data = mnb_bh_df, aes(x, y, group = group, fill = mytype)) +
        # Draw the habitat polygons
        geom_polygon()+
        # Set the colors
        scale_fill_manual("Habitat",values=c("Land"="black","Seagrass"="green",
                                             "CoralReef"="coral","Other"="yellow",
                                             "Artificial"="blue","Pavement"="orange"))+
        # Draw the MCPs
        geom_polygon(data=mcp_95, aes(x=x,y=y,group=transmitter, fill=NA),color="black")+
        # Set the extent
        coord_equal(xlim = c(XMin,XMax),
                    ylim = c(YMin,YMax)) +
        # Other formatting
        xlab("")+ ylab("")+
        theme(panel.background=element_blank(),
              axis.text = element_blank(), axis.ticks = element_blank())
# Save it for future reference
ggsave(paste0(sinkPath,"benthichab_MCPs.jpeg"),width = 6, height = 5, units = "in")
