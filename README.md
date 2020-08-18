This study is not yet published.

**Study:** Home range characteristics and diel patterns in space use of mutton snapper, *Lutjanus analis*, in St. Thomas, US Virgin Islands

**Authors:** Sarah L. Heidmann, Jonathan Jossart, Richard S. Nemeth\
Corresponding author: SL Heidmann, sarah.heidmann\@uvi.edu

**Abstract**\
Background: The movement ecology of mutton snapper *Lutjanus analis* is poorly understood despite their ecological and economic importance in the Caribbean. Passive acoustic telemetry was used to determine home ranges of six adult L. analis, including diel patterns, in Brewers Bay, St. Thomas, US Virgin Islands. Understanding long-term space use, including site fidelity and habitat usage, is necessary to implement effective and appropriate management actions for a species with comprehensive space and resource needs.\
Results: Individual *L. analis* were tracked over an average period of 316 days (range 125 - 509 days) and showed high site fidelity to relatively small home ranges (mean ± SD: 0.103 ± 0.028 km^2^, range 0.019 - 0.190 km^2^) and core use areas with low overlap among individuals. Most home ranges had a habitat composition dominated by seagrass and to a lesser degree, coral reef and/or pavement. Nighttime activity spaces were distinct from but contained within daytime areas.\
Conclusions: Mutton snapper showed strong site fidelity to home ranges in Brewers Bay. Two individuals that were absent from the array for more than a few hours were detected at separate arrays at spawning aggregation sites. This study expands upon knowledge of snapper home range characteristics, highlights the importance of maintaining adjacent high-quality habitat types in any spatial management plan, and encourages the adoption of other types of management strategies, particularly for transient-aggregating species.\
Keywords: acoustic telemetry, home range, US Virgin Islands, movement ecology, Lutjanidae, *Lutjanus analis*

**Analysis Scripts**\
Note: these scripts rely heavily on the tidyverse.
- *1_processing.R* - formats the raw data
- *2_cutting.R* - trims datasets of behavioral abnormalities
- *3_binning.R* - bins detections into 30-min periods
- *transum.R* - creates a detection summary table
- *detectionrates.R* - compare daytime and nighttime detection rates
- *plotrecdetday.R* - creates a plot of transmitter activity and temperature over time
- *mcp_creation.R* - creates minimum convex polygons for each fish
- *mcp_plotting.R* - plots minimum convex polygons for all fish
- *bbmm_creation.R* - creates Brownian bridge movement models for each fish
- *bbmm_plotting.R* - plots Brownian bridge movement model rasters for all fish
- *mcp_bbmm_plotting.R* - alternative plot with MCP and BBMM home ranges combined
- *mcp_metrics.R* - calculates sizes and separation of minimum convex polygons for each fish
- *mcp_spawning.R* - calculates sizes of minimum convex polygons including spawning sites
- *mcp_habitats.R* - calculates habitat composition of minimum convex polygons for each fish
- *habitat_plotting.R* - visually explores Brewers Bay habitats with and without MCPs
- *permanova.R* - performs a repeated measures PERMANOVA on space use composition
- *temperature_analysis.R* - a frequency analysis of temperature associations

