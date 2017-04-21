#==================================================================================================
#Project Name: NCEAS SIZE AT AGE - Generalized Additive Models
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 4.19.17
#
#Purpose: Evaluate Changes in Size at Age Using Generalized Additive Mixed Models
#
# 1) 
#
#
#==================================================================================================
#NOTES:
#  a) 
#
#==================================================================================================


days <- 150:240
n.days <- length(days)



#Distribution parameters
mu <- 200
sd <- 25

proc.sigma <- 0.2


#Parameters for trend across season
int <- 550
slp <- -2

#Generate size trend
size.trend <- int + slp*(days-mean(days))

#Define sampling periods


dates.pds <- matrix(c(160,170,
                      175,180,
                      185,190,
                      200,220), nrow=4, ncol=2, byrow=TRUE)

n.pds <- nrow(dates.pds)

#Samples per period
n.samps <- c(100,100,100,100)

if(length(n.samps)!=n.pds) { stop('#Number of Sampling periods does not equal length of sample sizes') }


#Arrival trend
N.dist <- dnorm(days, mu, sd)
N <- N.dist * rlnorm(n.days, 0-((proc.sigma^2)/2), proc.sigma)


#Plot Arrival and sampling Process
y.lim <- c(min(N.dist,N),max(N.dist,N))
plot(x=days, y=N.dist, type='l', col='red', ylim=y.lim)
points(x=days, y=N, pch=21, bg=rgb(1,0,0,alpha=0.2))

#Plot the blocks
i <- 1
for(i in 1:n.pds) {
  polygon(x=c(dates.pds[i,1], dates.pds[i,1], dates.pds[i,2], dates.pds[i,2]), y=c(ylim, rev(ylim)), fill=rgb(1,1,1,alpha=0.1))
}

errbar ()

















