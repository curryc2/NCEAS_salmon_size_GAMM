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


#Readin for first time
#===================================
#Read in data
load('Data/BB_linked.RData')

#Add year
dat$year <- year(dat$sampleDate)

# dat$sampleDate <- as.character(dat$sampleDate)
#Add doy
dat$doy <- yday(dat$sampleDate)

#Change ocean age to factor
dat$Salt.Water.Age <- as.factor(dat$Salt.Water.Age)

#Pair down to single year
dat <- dat[dat$CatchOrEsc=='Catch' & dat$Species=='sockeye' & dat$Length<=800,] #Egegik

#Metadata
dists <- sort(unique(dat$District))
n.dists <- length(dists)

head(dat)

#Exploratory plot
g <- ggplot(dat, aes(x=doy, y=Length, color=year)) +
       theme_gray() +
       geom_point(alpha=) +
       stat_smooth() +
       facet_wrap(~District)
g
ggsave('Plots/Within Season Change Overview.eps', height=6, width=8, dpi=300)



#Linear mixed-effects model
#Ugashik
mod.lmer <- lmer(Length ~ Salt.Water.Age + (doy|year), data=dat[dat$District==321,])
summary(mod.lmer)


#Egegik

#Naknek-Kvichak


# Nushagak






