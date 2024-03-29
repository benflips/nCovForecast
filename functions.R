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


checkSameNumberOfCols <- function(a, b) {
  test <- ncol(a)==ncol(b)
  if (!test) {
    print(paste0('ERROR: ', deparse(substitute(a)), ' and ', deparse(substitute(b)), ' do not have the same number of columns (', ncol(a), ' vs. ', ncol(b), ')'))
  }
  test
}

checkSameNumberOfRows <- function(a, b) {
  test <- nrow(a)==nrow(b)
  if (!test) {
    print(paste0('ERROR: ', deparse(substitute(a)), ' and ', deparse(substitute(b)), ' do not have the same number of rows (', nrow(a), ' vs. ', nrow(b), ')'))
  }
  test
}

checkNAs <- function(a) {
  test <- sum(is.na(a))==0
  if (!test) {
   print(paste0('ERROR: There are NAs somewhere in the ', deparse(substitute(a)), ' data: ', sum(is.na(a))))
 }
 test
}


# function to check that all data are (semi-strictly) increasing over time
  # x is dataframe in JHU format
  # tolerance is how large an error we can live with
  # returns vector of rows that meet inclusion criteria
cumulantCheck <- function(x, tolerance = 0.25){
  d <- as.matrix(x[, dateCols(x)])
  rcFun <- function(y){
    out <- diff(y)/y[-length(y)]
    out[is.nan(out)] <- 0
    sum(out < (-tolerance) & y[-length(y)] > 100) # ignore bumps in early reporting
  }
  rowCheck <- apply(d, 1, rcFun)
  rowCheck == 0
}

# function to catch when regions stop reporting recoveries
recoveryCheck <- function(recoveries, infections, tolerance = 7){
  dR <- as.matrix(recoveries[, dateCols(recoveries)])
  dI <- as.matrix(infections[, dateCols(infections)])
  diffR <- dR-cbind(rep(0, nrow(dR)), dR[,-ncol(dR)]) # change in recoveries 
  diffI <- dI-cbind(rep(0, nrow(dI)), dI[,-ncol(dI)]) # change in infections
  test <- diffR == 0 & diffI != 0 & dR != dI # no change in recoveries, and change in infections, and recoveries not equal to infections
  test <- test[, (ncol(test)-9): ncol(test)] # last ten days
  apply(test, 1, sum) > tolerance
}


# Function to load data and standardise names, columns and so on
loadData <- function(path){
  d <- read.csv(file = path, stringsAsFactors = FALSE) # read data
  names(d) <- gsub(pattern = "_", replacement = ".", x = names(d))
  d <- d[, colnames(d) %in% c("Country.Region", "Province.State") | dateCols(d)] # throw away unwanted columns
  names(d)[dateCols(d)] <- gsub(pattern = "X", replacement = "", x = names(d)[dateCols(d)])
  d <- d[c(2, 1, 3:ncol(d))] # reorder so country.region is first
  # remove unwanted rows
  unwanted <- c("Diamond Princess", "MS Zaandam") # cruise ships
  d <- subset(d, !(d$Country.Region %in% unwanted | d$Province.State %in% unwanted))
  # rename Burma
  d$Country.Region[d$Country.Region=="Burma"] <- "Myanmar"
  d <- emancipate(d) # elevate semi-autonomous colonies to countries for reporting
  d
}


# Takes infection, death, and recovrey time series and converts to active cases
  # returns standardised dataframes
activeCases <- function(infections, deaths, recoveries){
  ssCol <- dateCols(infections) # get date columns
  test1 <- checkSameNumberOfRows(infections,deaths)
  test2 <- checkSameNumberOfRows(infections,recoveries)
  if (test1) {
    # Standardise order
    infections <- infections[order(infections$Region),]
    deaths     <- deaths[order(deaths$Region),]
    recoveries <- recoveries[order(recoveries$Region),]
    orderTest <- sum(!(infections$Region == deaths$Region & infections$Region == recoveries$Region)) != 0
    if (orderTest) stop("Region labels do not align")
    # check for countries with inadequate reporting of recoveries, and apply recLag estimation
    recCheck <- recoveryCheck(recoveries, infections, tolerance = 7)
    recoveries[recCheck,] <- recLag(infections[recCheck,], deaths[recCheck,], active = FALSE)
    # subset to case data
    infMat <- infections[,ssCol]
    deathMat <- deaths[,ssCol]
    recMat <- recoveries[,ssCol]
    # generate active case data frame
    active <- infections
    active[,ssCol] <- infMat - deathMat - recMat
    # return standardised data frames
    list(tsI = infections, 
         tsD = deaths, 
         tsR = recoveries, 
         tsA = active, 
         failedRecovery = infections[recCheck, 1:2])
  } else {
    stop('Error - activeCases failed')
  }
}

# Adjusts cumulative infections to get active cases
  # cumulative infections and deaths, ttr = time to recovery
  # if active = false, returns recoveries rather than active cases
recLag <- function(infections, deaths, datCols = dateCols(infections), ttr = 22, active = TRUE, ignore.deaths=FALSE){
  matI<-as.matrix(infections[, datCols])
  matD<-as.matrix(deaths[, datCols])
  if (ignore.deaths) {
    matA <- matI #calculate 'removed' instead of 'recovered'
  } else {
    matA<-matI-matD #remove deaths
  }
  if (length(ttr)==1){
    matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA)), drop = FALSE]) # "recovered"
  } else {
    if (length(ttr) != nrow(infections)) stop("ttr vector is not of correct length")
    matR <- matrix(NA, nrow = nrow(matI), ncol = ncol(matI))
    for (ii in 1:nrow(matR)){
      matR[ii,] <- c(rep(0, ttr[ii]), matA[ii, -((ncol(matA)-(ttr[ii]-1)):ncol(matA)), drop = FALSE]) # lagged by theta days
    }
  }
  matA <- matA - matR
  if (ignore.deaths) {
    matR <- matR - matD #recovered = 'removed' minus deaths
  }
  if (active) {
    out <- data.frame(infections[,!datCols], matA) # "active" cases
  } else {
    out <- data.frame(infections[,!datCols], matR) # "recovered"
  }
  colnames(out) <- colnames(infections)
  out
}

# given time series data on recoveries, establishes optimum value for time to recovery in recLag function
recLagOptim <- function(infections, deaths, recoveries){
  errorFunc <- function(theta, infections, deaths, recoveries){
    activeTrue <- infections - deaths - recoveries
    estR <- infections - deaths # non-deaths
    estR <- c(rep(0, theta), estR[-((length(estR)-(theta-1)):length(estR))]) # lagged by theta days
    activeEst <- infections - deaths - estR
    ss <- activeTrue!=0 & activeEst>0
    sum(log(activeEst[ss]/activeTrue[ss])^2)/sum(ss) # mean squared error (log transformed data)
  }
  out <- round(optimize(f = errorFunc, interval = c(10, 32), infections=infections, deaths=deaths, recoveries=recoveries)$minimum, 0) # get the optimum
  if (out==32) out <- 23 # catch cases where optimize has crashed
  out
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

# same as above but on column labelled 'Region'
regionAggregate <- function(x) {
  xSelect <- x[, dateCols(x)]
  out <- aggregate(xSelect, by = list(Region = x$Region), sum)
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
  deathDiff[deathDiff<0] <- 0 # catch negative deaths (which shouldn't exist, but...)
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
  if (sum(rawN[ss]==0)>=(length(rawN[ss])-2)){ #if there are two or fewer non-zero data points...
    yPieces <- rep(0, length(x))
    y <- data.frame(fit = yPieces, lwr = yPieces, upr = yPieces)
    return(list(lDat = data.frame(dates = x, y), date_at_peak = NULL, value_at_peak = -99, doubling_time = Inf))
  }
  lnN <- log(rawN[ss]) 
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  if (timeVaryingGrowth){
    mFit <- lm(lnN~poly(tIn, 2))
    fit_based_on_integers_instead_of_dates <- lm(lnN ~ I(ss) + I(ss^2))
    intercept <- summary(fit_based_on_integers_instead_of_dates)$coefficients[1,1]
    poly1     <- summary(fit_based_on_integers_instead_of_dates)$coefficients[2,1]
    poly2     <- summary(fit_based_on_integers_instead_of_dates)$coefficients[3,1]
    if (poly2 != 0){
      date_at_peak <- tail(tIn,n=1) + (round(-poly1 / (2*poly2)) - nn)
      value_at_peak <- exp(intercept)*exp(poly1^2/(2*(-poly2)))*exp(poly2*(poly1/(2*poly2))^2)
    
      if (poly2 > 0 | date_at_peak > max(x)) {
       value_at_peak <- NULL
      }
      if (date_at_peak < min(x)){
        date_at_peak <-NULL
      }
    } else if (poly1 == 0 & intercept == 0) {
      value_at_peak <- -99
      date_at_peak <-NULL
    } else {
      value_at_peak <- -99
      date_at_peak <- NULL
    }
  } else {
    mFit <- lm(lnN~tIn)
    r <- coefficients(mFit)[2]
    doubling_time <- log(2)/r
  }
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence")
  y <- exp(extFit)
  if(timeVaryingGrowth) {
    list(lDat = data.frame(dates = x, y), date_at_peak = date_at_peak, value_at_peak = value_at_peak)
  } else {
    list(lDat = data.frame(dates = x, y), doubling_time = doubling_time)
  }

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

# Elevates semi-autonomous colonies to countries for reporting
emancipate <- function(timeSeriesDataFrame, withHead = TRUE){
  colonials <- timeSeriesDataFrame$Country.Region %in% c("Netherlands", "United Kingdom", "France", "Denmark")
  province <-  timeSeriesDataFrame$Province.State != ''
  ss <- colonials & province
  if (withHead) {
    timeSeriesDataFrame$Country.Region[ss] <- paste0(timeSeriesDataFrame$Province.State[ss], " (", timeSeriesDataFrame$Country.Region[ss], ")")
  } else {
    timeSeriesDataFrame$Country.Region[ss] <- timeSeriesDataFrame$Province.State[ss]
  }
  timeSeriesDataFrame$Province.State[ss] <- ""
  timeSeriesDataFrame
}

# Removes the Ruby Princess accounting that drops 189 cases into NSW on 3rd July
fixNSW <- function(timeSeriesDataFrame){
  rr <- timeSeriesDataFrame$Country.Region=="Australia" & timeSeriesDataFrame$Province.State=="New South Wales"
  getJumpCol <- which(names(timeSeriesDataFrame)=="7.3.20")
  timeSeriesDataFrame[rr,(getJumpCol):ncol(timeSeriesDataFrame)] <- timeSeriesDataFrame[rr,(getJumpCol):ncol(timeSeriesDataFrame)]-189
  timeSeriesDataFrame
}
