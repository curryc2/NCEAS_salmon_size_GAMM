#==================================================================================================
#Project Name: NCEAS SIZE AT AGE - Simulate Removals from Time-Series
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 4.19.17
#
#Purpose: Evaluate How Removals from Time-series will influence ability to define common smoothed process
#
# 1) Resample from Bristol Bay ASL data
# 2) With different # of years removed and level of autocorrelation in removal pattern (even vs. clumpy)
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
# dat <- read.csv('Data/BB_linked.csv', header=TRUE, stringsAsFactors=FALSE)

#Save as R data object
# save(dat, file='Data/BB_linked.RData')

load('Data/BB_linked.RData')

#Add year
dat$year <- year(dat$sampleDate)

#Remove the 


#Pair down to single year
temp.dat <- dat[dat$CatchOrEsc=='Catch' & dat$District==322 & dat$Species=='sockeye',] #Egegik

#Combine to the annual level
yr.dat <- data.frame(temp.dat %>% group_by(year,Salt.Water.Age) %>% 
                       summarize('mean.len'=mean(Length, na.rm=TRUE), 'sd.len'=sd(Length, na.rm=TRUE), 
                                 'up.90.len'=quantile(Length, probs=0.9, na.rm=TRUE)))



#Remove the NA's
yr.dat <- na.omit(yr.dat)

#Number of years
years <- sort(unique(yr.dat$year))
n.years <- length(years)

#===================================================================
# SEQUENTIAL REMOVAL

trial.rem <- 0:40#seq(0,40,by=5)
n.trial.rem <- length(trial.rem)

#For a specific age class
ages <- c(2:3)#sort(unique(yr.dat$Salt.Water.Age))
n.ages <- length(ages)

#Resamples
n.resamp <- 25

#Trial samples
samples <- array(data=NA,dim=c(n.ages,n.trial.rem,n.resamp,n.years), dimnames=list(ages,trial.rem,1:n.resamp,years))

#Generate resampled data sets
a <- 1
for(a in 1:n.ages) {
  print(a)
  t <- 1
  for(t in 1:n.trial.rem) {
    rem <- trial.rem[t]
    
    r <- 1
    for(r in 1:n.resamp) {
      temp.yrs <- sort(sample(years, n.years-rem, replace=FALSE))
      loc.yrs <- which(years %in% temp.yrs)
      
      y <- 1
      for(y in 1:length(loc.yrs)) {
        samples[a,t,r,loc.yrs[y]] <- yr.dat$mean.len[yr.dat$Salt.Water.Age==ages[a] & yr.dat$year==temp.yrs[y]]
      }#next y
    }#next r
  }#next t
}#next a



#Change to list format
require(reshape2)
samples.list <- melt(samples)
names(samples.list) <- c('o.age','n.rem','resamp','year','value')


o2.list <- samples.list[samples.list$o.age==2,]
o3.list <- samples.list[samples.list$o.age==3,]

#
#Loop thorugh removals

slp.mean <- array(dim=c(n.trial.rem,n.ages), dimnames=list(trial.rem, ages))
slp.sd <- array(dim=c(n.trial.rem,n.ages), dimnames=list(trial.rem, ages))

a <- 1
for(a in 1:n.ages) {
  t <- 1
  for(t in 1:n.trial.rem) {
    temp.data <- samples.list[samples.list$o.age==ages[a] & samples.list$n.rem==trial.rem[t],]
    temp.data <- na.omit(temp.data)
  
    temp.coef <- vector(length=n.resamp)
  
    i <- 1
    for(i in 1:n.resamp) {
      temp.coef[i] <- coef(lm(value ~ year, data=temp.data[temp.data$resamp==i,]))[2]
    }#next i
    #Define summary of coefficients
    slp.mean[t,a] <- mean(temp.coef)
    slp.sd[t,a] <- sd(temp.coef)
  
    # temp.lmer <- lmer(value ~ (year|resamp), data=temp.data)
  }
}#next a

slp.mean
slp.sd
slp.cv <- abs(slp.sd/slp.mean)


#Change in tandard Deviation
list.slp.sd <- melt(slp.sd)

head(list.slp.sd)
names(list.slp.sd) <- c('Removed','OceanAge','value')
list.slp.sd$OceanAge <- as.factor(list.slp.sd$OceanAge)

g <- ggplot(list.slp.sd, aes(x=Removed, y=value, color=OceanAge)) +
      geom_point() +
      theme_gray() +
      geom_smooth()
g

#Change in CV
list.slp.cv<- melt(slp.cv)

head(list.slp.cv)
names(list.slp.cv) <- c('Removed','OceanAge','value')
list.slp.cv$OceanAge <- as.factor(list.slp.cv$OceanAge)

g <- ggplot(list.slp.cv, aes(x=Removed, y=value, color=OceanAge)) +
  geom_point() +
  theme_gray() +
  geom_smooth() +
  xlab(paste('Number of Years Removed of',n.years)) +
  ylab('CV of Slope Estimates')
g
ggsave('Plots/CV of Slope as Fxn of Removals.png', plot=g, height=6, width=8)








plt.dat <- yr.dat
plt.dat$Salt.Water.Age <- as.factor(plt.dat$Salt.Water.Age)
g <- ggplot(plt.dat, aes(x=year, y=mean.len, color=Salt.Water.Age)) +
       geom_point() + 
       stat_smooth()
g

#Add sampling period number
samps <- sort(unique(temp.dat$doy, na.rm=TRUE))
n.samps <- length(samps)







