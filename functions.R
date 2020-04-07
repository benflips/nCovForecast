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

# Aggregates a particular country within a dataframe
  # useful for standardising the input dataframes
internalAgg <- function(tsDF, country){
  ssCol <- dateCols(tsDF)
  temp <- subset(tsDF, tsDF$Country.Region==country)
  agg <- countryAgg(temp)
  bRow <- temp[1,]
  bRow[ssCol] <- agg[,-1]
  tsDF <- subset(tsDF, tsDF$Country.Region!=country)
  rbind(tsDF, bRow)
}

# Takes infection, death, and recovrey time series and converts to active cases
  # returns standardised dataframes
activeCases <- function(infections, deaths, recoveries){
  ssCol <- dateCols(infections) # get date columns
  # find countries where recoveries have been aggregated, but infections/deaths have not
  countI <- table(infections$Country.Region)
  countR <- table(recoveries$Country.Region)
  cNames <- names(countI)[countI>countR]
  for (cc in cNames){
    # aggregate infections
    infections <- internalAgg(infections, cc)
    # aggregate deaths
    deaths <- internalAgg(deaths, cc)
  }
  # Standardise order
  infections <- infections[order(infections$Country.Region, infections$Province.State),]
  deaths <- deaths[order(deaths$Country.Region, deaths$Province.State),]
  recoveries <- recoveries[order(recoveries$Country.Region, recoveries$Province.State),]
  # subset to case data
  infMat <- infections[,ssCol]
  deathMat <- deaths[,ssCol]
  recMat <- recoveries[,ssCol]
  # generate active case data frame
  active <- infections
  active[,ssCol] <- infMat - deathMat - recMat
  # return standardised data frames
  list(tsI = infections, tsD = deaths, tsR = recoveries, tsA = active)
}

# Adjusts cumulative infections to get active cases
  # cumulative infections and deaths, ttr = time to recovery
recLag <- function(infections, deaths, datCols = dateCols(infections), ttr = 22){
  matI<-as.matrix(infections[, datCols])
  matD<-as.matrix(deaths[, datCols])
  matA<-matI-matD #remove deaths
  matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # recovered
  matA <- matA - matR
  
  out <- data.frame(infections[,!datCols], matA) # active cases
  colnames(out) <- colnames(infections)
  out
}

# calculates doubling time over the last inWindow days.
doubTime <- function(cases, time, inWindow = 10){
  r <- projSimpleSlope(cases, time, inWindow = inWindow)[2]
  log(2)/r
}


# growth rate
growthRate <- function(cases, inWindow=10){
  nn <- length(cases)
  ss <- (nn - inWindow + 1):nn
  rate <- numeric(length(ss))
  rate[ss] <- 100 * (cases[ss] - cases[ss-1]) / cases[ss-1]
}


# aggregates results to country
countryAgg<-function(x){
  xSelect<-x[, dateCols(x)]
  aggregate(xSelect, by = list(Country = x$Country.Region), FUN = sum)
}

# calculates a nation aggregate and appends to dataframe
natAgg <-function(tsDF){
  cAgg <- tsSub(tsDF, subset = tsDF$Country.Region==tsDF$Country.Region[1])
  dim(cAgg)<-c(1, length(cAgg))
  cAgg<-data.frame(tsI[1, !dCols], cAgg)
  cAgg$Province.State <- "National aggregate"
  colnames(cAgg)<-colnames(tsDF)
  rbind(cAgg, tsDF)
}

# calculates the curve flatenning index.
  # it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  lnact <-log(active)
  cfiInd <- -diff(diff(lnact))/abs(diff(lnact)[-1])
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}

# estimates detection rate based on assumptions about cfr, ttd
detRate<-function(infd, deaths, cfr = 0.033, ttd=17, window=5){
  obs<-c(rep(NA, window), diff(infd, window)) # observed new cases
  deathDiff<-diff(deaths, window) # observed new deaths
  expd<-deathDiff/cfr #expected new cases given cfr
  expd<-expd[-(1:(ttd-window))]
  expd<-c(expd, rep(NA, ttd))
  detRate<-obs/expd
  detRate[detRate==0]<-NA
  detRate[is.infinite(detRate)]<-NA
  out<-mean(detRate, na.rm = TRUE)
  if (is.nan(out)) return(NA)
  if (out>1) out<-1
  out
}

# Simple projection based on growth over last inWindow days
  # returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10, extWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:extWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence")
  y <- exp(extFit)
  data.frame(dates = x, y)
}

# Simple projection based on growth over last inWindow days
# returns coefficients
projSimpleSlope<-function(rawN, rawTime, inWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  coefficients(mFit)
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

