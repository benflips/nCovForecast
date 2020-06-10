getDataGeneral <- function(countryName, inputConfirmed, inputDeaths, inputRecovered, aggregateOverProvinceState, isThisAFocusCountry){
## ---------------------------##
## Script name: getDataGeneral.R
##
## Purpose of script: to get data from a given set up confirmed cases, deaths, and (if it exists) recoveries.
##
## load up our functions into memory
source('functions.R')

cat("\n")
print('-----------------------')
print('-----------------------')
print(paste('      ',countryName))
print('-----------------------')
print('-----------------------')

noErrors <- TRUE

timeSeriesInfections <-loadData(inputConfirmed)
timeSeriesDeaths     <-loadData(inputDeaths)

if (inputRecovered != '') {
  timeSeriesRecoveries <- loadData(inputRecovered)
}

if (isThisAFocusCountry) {
  timeSeriesInfections <- subset(timeSeriesInfections, timeSeriesInfections$Country.Region == countryName)
  timeSeriesDeaths     <- subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     == countryName)
  if (inputRecovered != '') {
    timeSeriesRecoveries <- subset(timeSeriesRecoveries, timeSeriesRecoveries$Country.Region == countryName)
  }
}

rm(inputConfirmed, inputDeaths, inputRecovered)

# aggregate data to Province.State
if (aggregateOverProvinceState) {
  timeSeriesInfections <-regionAgg(timeSeriesInfections, regionCol = timeSeriesInfections$Province.State, regionName = "Province.State")
  timeSeriesInfections$Country.Region <- rep(countryName, nrow(timeSeriesInfections))
  timeSeriesInfections <- timeSeriesInfections[c(ncol(timeSeriesInfections), 1:(ncol(timeSeriesInfections)-1))]

  timeSeriesDeaths <-regionAgg(timeSeriesDeaths, regionCol = timeSeriesDeaths$Province.State, regionName = "Province.State")
  timeSeriesDeaths$Country.Region <- rep(countryName, nrow(timeSeriesDeaths))
  timeSeriesDeaths <- timeSeriesDeaths[c(ncol(timeSeriesDeaths), 1:(ncol(timeSeriesDeaths)-1))]
}


# test structural integrity of data
test1 <- checkSameNumberOfRows(timeSeriesInfections, timeSeriesDeaths)
test2 <- checkSameNumberOfCols(timeSeriesInfections, timeSeriesDeaths)

test3 <- checkNAs(timeSeriesInfections)
test4 <- checkNAs(timeSeriesDeaths)

noErrors <- noErrors & test1 & test2 & test3 & test4

if (exists('timeSeriesRecoveries')) {

  test5 <- checkSameNumberOfCols(timeSeriesInfections, timeSeriesRecoveries)

  test6 <- checkNAs(timeSeriesRecoveries)

  noErrors <- noErrors & test5 & test6

}

if (noErrors) {

  # All tests passed successfully
  
  ## get date range 
  dCols<-dateCols(timeSeriesInfections)
  dates<-as.Date(colnames(timeSeriesInfections)[dCols], format = "%m.%d.%y")
  
  ## generate recovery data if it doesn't already exist
  if (!exists('timeSeriesRecoveries')) {
    timeSeriesRecoveries <- recLag(timeSeriesInfections, timeSeriesDeaths, active = FALSE)
  }

  # Standardise dataframes and compute active cases
  std <- activeCases(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries)
  
  # report to console countries that have been recLagged within activeCases()
  if (nrow(std$failedRecovery)>0) {
    print(paste("There are", nrow(std$failedRecovery), "region(s) failing recovery data test and treated with recLag: "))
    print(std$failedRecovery)
    cat("\n\n")
  }

  
  # exclude data where there are large errors in the infection and death cumulants
  checkI <- cumulantCheck(std$tsI)
  checkD <- cumulantCheck(std$tsD)
  cumSub <- checkI & checkD
  if (sum(!cumSub)>5) stop("More than five suspect regions in this dataset.")
  if (sum(!cumSub)>0) {
    print(paste("States excluded through failed cumulants:", sum(!cumSub)))
    print(cbind(std$tsI[!cumSub, 1:2], checkI = checkI[!cumSub], checkD = checkD[!cumSub]))
    cat("\n\n")
  }
  tsI <- std$tsI[cumSub,]
  tsD <- std$tsD[cumSub,]
  tsR <- std$tsR[cumSub,]
  tsA <- std$tsA[cumSub,]
  rm(checkI, checkD, cumSub)
  
  print("Organising data...")
  
  # aggregate to region
  tsI <- regionAgg(tsI, regionCol = tsI$Province.State)
  tsD <- regionAgg(tsD, regionCol = tsD$Province.State)
  tsR <- regionAgg(tsR, regionCol = tsR$Province.State)
  tsA <- regionAgg(tsA, regionCol = tsA$Province.State)
  
  timeSeriesInfections <- natAgg(tsI, aggName = paste("Aggregate -", countryName))
  timeSeriesDeaths     <- natAgg(tsD, aggName = paste("Aggregate -", countryName))
  timeSeriesRecoveries <- natAgg(tsR, aggName = paste("Aggregate -", countryName))
  timeSeriesActive     <- natAgg(tsA, aggName = paste("Aggregate -", countryName))
  
  if (focusCountry == 'Global') {
    # create global aggregate row
    timeSeriesInfections <- natAgg(timeSeriesInfections, aggName = "Global aggregate")
    timeSeriesDeaths     <- natAgg(timeSeriesDeaths,     aggName = "Global aggregate")
    timeSeriesRecoveries <- natAgg(timeSeriesRecoveries, aggName = "Global aggregate")
    timeSeriesActive     <- natAgg(timeSeriesActive,     aggName = "Global aggregate")
  }



  ## Define menus
  # get region names with 20 or more cases as of yesterday
  ddNames <- timeSeriesInfections$Region[timeSeriesInfections[[ncol(timeSeriesInfections)-1]]>19]
  ddReg        <- ddNames
  names(ddReg) <- ddNames
  
  ## write data caches out
  dir.create(paste0("dat/", countryName), showWarnings = FALSE) # if the directory doesn't exist, create it.
  save(ddReg, ddNames, file = paste0("dat/",countryName,"/menuData.RData"))
  save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = paste0("dat/",countryName,"/cacheData.RData"))

  # un comment these lines to run deconvolution (SLOW!)  
  system(paste("Rscript detection/estGlobalV2.R", countryName), wait = TRUE)
  load(paste0("dat/",countryName,"/estDeconv.RData"))
  
  # load dataList object
  load("dat/dataList.RData")
  
  # append data to dataList
  dataList[[countryName]] <- list(timeSeriesInfections = timeSeriesInfections,
                                   timeSeriesDeaths = timeSeriesDeaths,
                                   timeSeriesRecoveries = timeSeriesRecoveries,
                                   timeSeriesActive = timeSeriesActive,
                                   dates = dates,
                                   ddReg = ddReg,
                                   ddNames = ddNames,
                                   cumulative.infections = cumulative.infections,
                                   undiagnosed.infections = undiagnosed.infections, 
                                   active.projections = active.projections,
                                   timestampRan = Sys.time())
  
  # write datList back out
  save(dataList, file = "dat/dataList.RData")
  print("Complete")
} else {
  print('No data was saved')
}

}
