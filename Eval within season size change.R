#==================================================================================================
#Project Name: NCEAS SIZE AT AGE - Evaluate wheter weighting is necessary, by looking at within season trends in size @ age
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 4.20.17
#
#Purpose: Overall goal is to partition change in size over time between: 1) Change in age composition, 2) Change in growth (size@age)
#
#
#
#==================================================================================================
#NOTES:
#
#==================================================================================================
require(tidyverse)
require(lme4)
require(gamm4)
require(reshape2)


#Read in data
dat <- read.csv('Data/asl_size_age.csv', header=TRUE)







