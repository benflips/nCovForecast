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

# Function to load data and standardise names, columns and so on
loadData <- function(path){
  d <- read.csv(file = path, stringsAsFactors = FALSE) # read data
  names(d) <- gsub(pattern = "_", replacement = ".", x = names(d))
  d <- d[, colnames(d) %in% c("Country.Region", "Province.State") | dateCols(d)] # throw away unwanted columns
  names(d)[dateCols(d)] <- gsub(pattern = "X", replacement = "", x = names(d)[dateCols(d)])
  d <- d[c(2, 1, 3:ncol(d))] # reorder so country.region is first
  # remove unwanted rows
  unwanted <- c("Diamond Princess", "MS Zaandam")
  d <- subset(d, !(d$Country.Region %in% unwanted | d$Province.State %in% unwanted))
  # rename Burma
  d$Country.Region[d$Country.Region=="Burma"] <- "Myanmar"
  d
}


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
  # if active = false, returns recoveries rather than active cases
recLag <- function(infections, deaths, datCols = dateCols(infections), ttr = 22, active = TRUE){
  matI<-as.matrix(infections[, datCols])
  matD<-as.matrix(deaths[, datCols])
  matA<-matI-matD #remove deaths
  matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # "recovered"
  matA <- matA - matR
  if (active) {
    out <- data.frame(infections[,!datCols], matA) # "active" cases
  } else {
    out <- data.frame(infections[,!datCols], matR) # "recovered"
  }
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


# aggregates results to relevant region (regionCol allows to specify whether Provinc.State, or COuntry.Region)
regionAgg<-function(x, regionCol, regionName = "Region"){
  xSelect<-x[, dateCols(x)]
  out <- aggregate(xSelect, by = list(regionCol), FUN = sum)
  names(out)[1] <- regionName
  out
}


# calculates a national aggregate and appends to dataframe
  # must be called after regionAgg
natAgg <-function(tsDF, aggName = "National aggregate"){
  cAgg <- colSums(tsDF[,-1])
  dim(cAgg)<-c(1, length(cAgg))
  cAgg <- data.frame(Region = aggName, cAgg, stringsAsFactors = FALSE)
  colnames(cAgg) <- colnames(tsDF)
  rbind(cAgg, tsDF)
}

# aggregates to ocontinents
  # called after regionAgg and natAgg takes case data frame and continent datafram
  # returns case data fram with continents in rows 2-7
continentAgg <- function(tsDF, contData){
  ts <- subset(tsDF, tsDF$Region %in% contData$Country)
  ts$Region <- contData$Continent[match(ts$Region, contData$Country)]
  ts <- regionAgg(ts, regionCol = ts$Region)
  rbind(tsDF[1,], ts, tsDF[-1,])
}

# calculates the curve flatenning index.
  # it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  lnact <-log(active)
  cfiInd <- -diff(diff(lnact))/abs(diff(lnact)[-1])
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}

# estimates detection rate based on assumptions about caseFatalityRatio, ttd
detRate<-function(infd, deaths, caseFatalityRatio = 3.3, ttd=17, window=5, pointEst = TRUE){
  obs<-c(rep(NA, window), diff(infd, window)) # observed new cases
  deathDiff<-diff(deaths, window) # observed new deaths
  expd<-deathDiff/(caseFatalityRatio/100) #expected new cases given caseFatalityRatio
  expd<-expd[-(1:(ttd-window))]
  expd<-c(expd, rep(NA, ttd))
  detRate<-obs/expd
  detRate[detRate==0]<-NA
  detRate[is.infinite(detRate)]<-NA
  if (pointEst) {
    out<-mean(tail(na.omit(detRate), 5), na.rm = TRUE) # take mean over last 5 observations
    if (is.nan(out)) return(NA)
    if (out>1) out <- 1
  } else {
    out <- detRate
    out[is.nan(out)] <- NA
    out[out>1] <- 1
    }
  out
}

# Simple projection based on growth over last inWindow days
  # returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10, extWindow=10, timeVaryingGrowth = FALSE){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:extWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  if (timeVaryingGrowth){
    mFit <- lm(lnN~poly(tIn, 2))
  } else {
    mFit <- lm(lnN~tIn)
  }
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
  grepl(pattern = "\\d.\\d", x = colnames(x))
}

# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}

