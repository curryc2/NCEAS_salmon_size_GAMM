#==================================================================================================
#Project Name: NCEAS SIZE AT AGE - Bristol Bay GAMM
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 4.21.17
#
#Purpose: To Evaluate Trends in Size @ Age Using Mixed GAMs
#
#
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================
require(tidyverse)
require(lme4)
require(gamm4)
require(reshape2)
#Read in Bristol Bay data

load('Data/BB_linked.RData')

#Add year
dat$year <- year(dat$sampleDate)

#Remove the 

#Pair down to single year
temp.dat <- dat[dat$CatchOrEsc=='Catch' & dat$Species=='sockeye',] #Egegik

#Combine to the annual level
yr.dat <- data.frame(temp.dat %>% group_by(year,Salt.Water.Age,District) %>% 
                       summarize('mean.len'=mean(Length, na.rm=TRUE), 'sd.len'=sd(Length, na.rm=TRUE), 
                                 'up.90.len'=quantile(Length, probs=0.9, na.rm=TRUE), 'n'=n()))

#Remove NA's
yr.dat <- na.omit(yr.dat)

#Name everything
names(yr.dat) <- c('year','SWage','Dist','mean.len','sd.len','up.90.len','n')

#Remove Nushagak for now
yr.dat <- yr.dat[yr.dat$District]











