#==================================================================================================
#Project Name: NCEAS SIZE AT AGE - Chain Rule to Partition Change in Size Due to Age v Growth
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 4.20.17
#
#Purpose: Overall goal is to partition change in size over time between: 1) Change in age composition, 2) Change in growth (size@age)
#
#
#
#==================================================================================================
#NOTES:
#  a) Chain Rule: d x.mu/dt = P_a * (dX_a/dt) + X_a * (dP_a/dt)
#
#==================================================================================================
require(tidyverse)
require(lme4)
require(gamm4)
require(reshape2)
require(lubridate)
#Read in Bristol Bay data

load('Data/BB_linked.RData')

#Add year
dat$year <- year(dat$sampleDate)

# dat$sampleDate <- as.character(dat$sampleDate)
#Add doy
dat$doy <- yday(dat$sampleDate)

#Remove the 

#Pair down to single year
temp.dat <- dat[dat$CatchOrEsc=='Catch' & dat$District==322 & dat$Species=='sockeye',] #Egegik


trial <- 



#Combine to the annual level
yr.dat <- data.frame(temp.dat %>% group_by(year,Salt.Water.Age) %>% 
                       summarize('mean.len'=mean(Length, na.rm=TRUE), 'sd.len'=sd(Length, na.rm=TRUE), 
                                 'up.90.len'=quantile(Length, probs=0.9, na.rm=TRUE), 'n'=n()))

#Remove NA's
yr.dat <- na.omit(yr.dat)

head(yr.dat)

years <- 1980:max(yr.dat$year)









