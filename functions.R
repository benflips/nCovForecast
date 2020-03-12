## ---------------------------
##
## Script name: functions.R
##
## Purpose of script: Hold a bunch of functions for coronaRisk app
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 

## ---------------------------

## function definitions

# estimates detection rate based on assumptions about cfr, ttd
detRate<-function(active, deaths){
  confirmed<-actNew(active)
  expected<-expNew(deaths)
  if (expected==0) return(1)
  detRate<-confirmed/expected
  names(detRate)<-"detRate"
  detRate
}


# gets number of new cases within window ttd days backwards in time
actNew<-function(active, ttd=17, window=5){
  nn<-length(active)
  nConf<-active[nn-ttd]-active[nn-ttd-window]
  nConf
}


# To use death rate in last 5 days to estimate expected number of new cases 17 days ago given cfr
expNew<-function(deaths, cfr=0.02, window=5){
  nn <- length(deaths)
  nDeaths <- deaths[nn]-deaths[nn-window]
  nDeaths/cfr
}

# Simple projection based on growth over last inWindow days
  # returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  lnN <- log(rawN[ss])
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "prediction")
  y <- exp(extFit)
  list(x=x, y=y)
}

# to identify the date columns in ts dataframes
dateCols<-function(x){
  grepl(pattern = "\\d", x = colnames(x))
}

# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}

