#==================================================================================================
#Project Name: NCEAS SIZE AT AGE - Generalized Additive Models
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 4.19.17
#
#Purpose: Evaluate Changes in Size at Age Using Generalized Additive Mixed Models
#
# 1) Read in data using FishData from throson
#
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================
require(mgcv)
require(ggplot2)
require(fda)
require(dplyr)
require(gamm4)


#Read in data
asl <- read.csv('Data/asl_size_at_age.csv', header=TRUE, stringsAsFactors=FALSE)
head(asl)


#Metadata
regions <- sort(unique(asl$SASAP.Region))
n.regions <- length(regions)

unique(asl$ASLProjectType)


#Aggreagate up to Region level (just for fun)

asl.agg <- data.frame(asl %>% group_by(SASAP.Region, Year, SWage, Species) %>% summarize(mean=mean(Length_mean, na.rm=TRUE)) %>% filter(mean!='NaN'))

asl.agg <- na.omit(asl.agg) #Remove NA's
#Example GAMM

g <- ggplot(asl.agg, aes(x=Year, y=mean, color=SASAP.Region)) +
       theme_gray() +
       geom_point() +
       facet_grid(Species~SWage)
g



#Basic GAM
gam.1 <- gam(mean~s(Year, bs='cr') + SWage + Species, data=asl.agg)
summary(gam.1)

acf(residuals(gam.1,type="response"),main="standardized residual ACF")



#GAMM with Region as RE'



gamm.1 <- gamm4(mean~s(Year, bs='cr') + SWage + Species, random=~(1|SASAP.Region), data=asl.agg)


gamm.2 <- gamm4(mean~s(Year, bs='cr') + SWage + Species, random=~(Year|SASAP.Region), data=asl.agg)




# 
# gam.1 <- gam(mean~s(Year, bs='cr'), correlation=corAR1(), data=asl.agg)
# 
# 
# trial.1 <- gamm(mean ~ s(Year, bs='cr'), random=list(data=asl.agg))
#                 
#                 
#               


#Creation of fourier baiss
years <- c(min(asl.agg$Year, na.rm=TRUE):max(asl.agg$Year, na.rm=TRUE))
n.years <- length(years)
fbs <- create.fourier.basis(rangeval=c(min(years),max(years)), n.basis=)
plot(fbs)  
         




ys <- smooth.basis(argvals=as.numeric(asl.agg$Year), y=asl.agg$mean, fdParobj=fbs)

plot(ys)



tempfd3 <- with(CanadianWeather, smooth.basis(
  day.5, dailyAv[,,"Temperature.C"],
  daybasis3, fdnames=list("Day", "Station", "Deg C"))$fd )




precfd5 <- with(CanadianWeather, smooth.basis(
  day.5, dailyAv[,,"log10precip"],
  daybasis5, fdnames=list("Day", "Station", "Deg C"))$fd )
# Correlation matrix for a single functional data object
(tempCor3 <- cor.fd(seq(0, 356, length=4), tempfd3))
# Cross correlation matrix between two functional data objects
# Compare with structure described above under 'value':





