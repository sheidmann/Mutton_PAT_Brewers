# length_regressions.R

# Sarah Heidmann
# Created 30 Jan 2021
# Last modified 30 Jan 2021

# Summary: performs regressions comparing length of Brewers Bay mutton snapper to home ranges

# Data inputs:
#     - table of fish sizes and home range characteristics
#         -fish total length
#         -sizes of 95% and 50% MCPs
#         -sizes of 95% and 50% BBMMs
#         -sizes of day and night activity spaces
#         -overlap index
# Actions:
#     - performs linear regressions of length vs each variable
# Data exports:
#     - NA

# Load the libraries
library(tidyverse)

##### Import the data #####
dat <- read_csv("data/otherdata/length_regressions.csv")

##### Regressions #####
# 95% MCP
ggplot(data = dat,
       aes(x=Length.cm, y=MCP_95)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(MCP_95~Length.cm, dat))

# 50% MCP
ggplot(data = dat,
       aes(x=Length.cm, y=MCP_50)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(MCP_50~Length.cm, dat))

# 95% BBMM
ggplot(data = dat,
       aes(x=Length.cm, y=BBMM_95)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(BBMM_95~Length.cm, dat))

# 50% BBMM
ggplot(data = dat,
       aes(x=Length.cm, y=BBMM_50)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(BBMM_50~Length.cm, dat))

# Daytime activity
ggplot(data = dat,
       aes(x=Length.cm, y=MCP_day)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(MCP_day~Length.cm, dat))

# Nighttime activity
ggplot(data = dat,
       aes(x=Length.cm, y=MCP_night)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(MCP_night~Length.cm, dat))

# overlap index
ggplot(data = dat,
       aes(x=Length.cm, y=OI_50)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y~x) +
     theme(panel.background=element_blank(),
           axis.line=element_line())
summary(lm(OI_50~Length.cm, dat))
