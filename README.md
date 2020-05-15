This study is not yet published.

**Study:** Home range characteristics and diel changes in space use of mutton snapper, Lutjanus analis, in St. Thomas, US Virgin Islands

**Authors:** Sarah L. Heidmann*1, Jonathan Jossart2, Richard S. Nemeth1

**Abstract**
Background: The movement ecology of mutton snapper *Lutjanus analis* is poorly understood despite their ecological and economic importance in the Caribbean. Passive acoustic telemetry was used to determine home ranges of six adult L. analis, including diel patterns, in Brewers Bay, St. Thomas, US Virgin Islands. Understanding long-term space use, including site fidelity and habitat usage, is necessary to implement effective and appropriate management actions for a species with comprehensive space and resource needs.
Results: Individual *L. analis* were tracked over an average period of 313 days (range 125 - 509 days) and showed high site fidelity to relatively small home ranges (mean ± SD: 0.112 ± 0.086 km^2^, range 0.017 - 0.238 km^2^) and core use areas that were distinct between individuals. Most home ranges had a habitat composition dominated by seagrass and to a lesser degree, coral reef, except one covering mainly coral reef habitat. Daytime and nighttime activity spaces were separate but highly overlapping.
Conclusions: Individuals were highly resident to territories in Brewers Bay; two that were absent from the array for more than a few hours were detected at separate arrays at spawning aggregation sites. Methodology affected home range estimations, both in size and shape. This study expands upon knowledge of snapper home range characteristics, highlights the importance of maintaining adjacent high-quality habitat types in any spatial management plan, and encourages the adoption of other types of management strategies, particularly for transient-aggregating species.
Keywords: acoustic telemetry, home range, US Virgin Islands, movement ecology, Lutjanidae, *Lutjanus analis*

**Analysis Scripts**
Note: these scripts rely heavily on the tidyverse and lubridate packages. There is a line in each one to load the appropriate libraries.
- *2_processing.R* - formats the raw data
- *3_cutting.R* - trims datasets of behavioral abnormalities
- *3b_transum.R* - creates a detection summary table
- *4_binning.R* - bins detections into 30-min periods

