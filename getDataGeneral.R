library(zipR)

getDataGeneral <- function(countryName, inputConfirmed, inputDeaths, inputRecovered, aggregateOverProvinceState, isThisAFocusCountry, verbose){
## ---------------------------##
## Script name: getDataGeneral.R
##
## Purpose of script: to get data from a given set up confirmed cases, deaths, and (if it exists) recoveries.
##
## load up our functions into memory
source('functions.R')

if (verbose) {
 cat("\n")
  print('-----------------------')
  print('-----------------------')
  print(paste('      ',countryName))
  print('-----------------------')
  print('-----------------------')
  print('')

  print(paste('Confirmed csv:', inputConfirmed))
  print(paste('Deaths csv:   ',    inputDeaths))
  print(paste('Recovered csv:', inputRecovered))
  print('')
}

noErrors <- TRUE

timeSeriesInfections <-loadData(inputConfirmed)
timeSeriesDeaths     <-loadData(inputDeaths)

if (verbose) {
  print(names(timeSeriesInfections)[1:4])
  print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
  print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
}

if (inputRecovered != '') {
  timeSeriesRecoveries <- loadData(inputRecovered)
  if (verbose) {
    print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
  }
  if (countryName == 'Global') {
    # only include rows with empty string for Province.State, and remove Province.State, and replace first column name with Region
    timeSeriesInfections <- subset(timeSeriesInfections, timeSeriesInfections$Province.State == '')
    timeSeriesDeaths     <- subset(timeSeriesDeaths,     timeSeriesDeaths$Province.State     == '')
    timeSeriesRecoveries <- subset(timeSeriesRecoveries, timeSeriesRecoveries$Province.State == '')
    timeSeriesRecoveries <- subset(timeSeriesRecoveries, timeSeriesRecoveries$Country.Region != 'Canada')
    timeSeriesInfections$Province.State <- NULL
    timeSeriesDeaths$Province.State     <- NULL
    timeSeriesRecoveries$Province.State <- NULL
    names(timeSeriesInfections)[1] <- 'Region'
    names(timeSeriesDeaths)[1]     <- 'Region'
    names(timeSeriesRecoveries)[1] <- 'Region'
    if (verbose) {
      print('')
      print('After excluding those with a Province.State (and, temporarily, Canada):')
      print(names(timeSeriesInfections)[1:4])
      print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
      print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
      print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
    }
  }
}

print('')

if (isThisAFocusCountry) {
  timeSeriesInfections <- subset(timeSeriesInfections, timeSeriesInfections$Country.Region == countryName)
  timeSeriesDeaths     <- subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     == countryName)
  timeSeriesInfections$Country.Region <- NULL
  timeSeriesDeaths$Country.Region     <- NULL
  names(timeSeriesInfections)[1] <- 'Region'
  names(timeSeriesDeaths)[1]     <- 'Region'
  if (inputRecovered != '') {
    timeSeriesRecoveries <- subset(timeSeriesRecoveries, timeSeriesRecoveries$Country.Region == countryName)
    timeSeriesRecoveries$Country.Region <- NULL
    names(timeSeriesRecoveries)[1] <- 'Region'
  }
  if (verbose) {
    print('After limiting to the focus country')
    print(names(timeSeriesInfections)[1:4])
    print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
    print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
    if (inputRecovered != '') {
      print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
    }
  }
}

rm(inputConfirmed, inputDeaths, inputRecovered)

# aggregate data to Province.State - this is done for US
if (aggregateOverProvinceState) {
  timeSeriesInfections <-regionAgg(timeSeriesInfections, regionCol = timeSeriesInfections$Province.State, regionName = "Province.State")
  timeSeriesInfections$Country.Region <- rep(countryName, nrow(timeSeriesInfections))
  timeSeriesInfections <- timeSeriesInfections[c(ncol(timeSeriesInfections), 1:(ncol(timeSeriesInfections)-1))]

  timeSeriesDeaths <-regionAgg(timeSeriesDeaths, regionCol = timeSeriesDeaths$Province.State, regionName = "Province.State")
  timeSeriesDeaths$Country.Region <- rep(countryName, nrow(timeSeriesDeaths))
  timeSeriesDeaths <- timeSeriesDeaths[c(ncol(timeSeriesDeaths), 1:(ncol(timeSeriesDeaths)-1))]

  if (verbose) {
    print('After aggregating over Province.State')
    print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
    print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
  }
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
  
  if (verbose) {
    print('')
    print('Function activeCases complete')
    print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
    print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
    print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
  }

  # report to console countries that have been recLagged within activeCases()
  if (nrow(std$failedRecovery)>0) {
    print(paste("There are", nrow(std$failedRecovery), "region(s) failing recovery data test and treated with recLag: "))
    print(std$failedRecovery)
    cat("\n\n")
  }

  
  # exclude data where there are large errors in the infection and death cumulants
  checkI <- cumulantCheck(std$tsI)
  checkD <- cumulantCheck(std$tsD)
  checkR <- cumulantCheck(std$tsR, tolerance = 0.5)
  cumSub <- checkI & checkD & checkR
  if (sum(!cumSub)>5) stop("More than five suspect regions in this dataset.")
  if (sum(!cumSub)>0) {
    print(paste("Regions excluded through failed cumulants:", sum(!cumSub)))
    print(cbind(std$tsI[!cumSub, 1:2], checkI = checkI[!cumSub], checkD = checkD[!cumSub], checkR = checkR[!cumSub]))
    cat("\n\n")
  }
  timeSeriesInfections <- std$tsI[cumSub,]
  timeSeriesDeaths     <- std$tsD[cumSub,]
  timeSeriesRecoveries <- std$tsR[cumSub,]
  timeSeriesActive     <- std$tsA[cumSub,]
  rm(checkI, checkD, checkR, cumSub)

  if (verbose) {
    print('')
    print('After excluding regions which failed cumulants')
    print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
    print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
    print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
  }



  if (countryName == 'Global') {
    # create global aggregate row
    timeSeriesInfections <- natAgg(timeSeriesInfections, aggName = "Global aggregate")
    timeSeriesDeaths     <- natAgg(timeSeriesDeaths,     aggName = "Global aggregate")
    timeSeriesRecoveries <- natAgg(timeSeriesRecoveries, aggName = "Global aggregate")
    timeSeriesActive     <- natAgg(timeSeriesActive,     aggName = "Global aggregate")

    if (verbose) {
      print('After aggregating to global')
      print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
      print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
      print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
      print(paste(toString(dim(timeSeriesActive)),     '- dimensions of active cases'))
      print(paste('First 5 headings of timeSeriesInfections:', toString(names(timeSeriesInfections)[1:5])))
    }

 } else {

    # aggregate to region
#    timeSeriesInfections <- regionAgg(timeSeriesInfections, regionCol = timeSeriesInfections$Province.State)
#    timeSeriesDeaths     <- regionAgg(timeSeriesDeaths,     regionCol = timeSeriesDeaths$Province.State)
#    timeSeriesRecoveries <- regionAgg(timeSeriesRecoveries, regionCol = timeSeriesRecoveries$Province.State)
#    timeSeriesActive     <- regionAgg(timeSeriesActive,     regionCol = timeSeriesActive$Province.State)
  
    if (verbose) {
      print('')
      print('After aggregating to region')
      print(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
      print(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
      print(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
      print(paste(toString(dim(timeSeriesActive)),     '- dimensions of active cases'))
      print(paste('First 5 headings of timeSeriesInfections', toString(names(timeSeriesInfections)[1:5])))
   }

    timeSeriesInfections <- natAgg(timeSeriesInfections, aggName = paste("National aggregate -", countryName))
    timeSeriesDeaths     <- natAgg(timeSeriesDeaths,     aggName = paste("National aggregate -", countryName))
    timeSeriesRecoveries <- natAgg(timeSeriesRecoveries, aggName = paste("National aggregate -", countryName))
    timeSeriesActive     <- natAgg(timeSeriesActive,     aggName = paste("National aggregate -", countryName))
  }


  ## Define menus
  # get region names with 20 or more cases as of yesterday
  ddNames <- timeSeriesInfections$Region[timeSeriesInfections[[ncol(timeSeriesInfections)-1]]>19]
print('those with more than 19 infections')
print(timeSeriesInfections[[ncol(timeSeriesInfections)-1]]>19)
print(timeSeriesInfections$Country.timeSeriesInfections[[ncol(timeSeriesInfections)-1]]>19)
  print('ddNames')
  print(ddNames)
  ddReg        <- ddNames
  names(ddReg) <- ddNames
  
  ## write data caches out
  dir.create(paste0("dat/", countryName), showWarnings = FALSE) # if the directory doesn't exist, create it.
  save(ddReg, ddNames, file = paste0("dat/",countryName,"/menuData.RData"))
  save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = paste0("dat/",countryName,"/cacheData.RData"))

  runDeconvolution <- FALSE
  if (runDeconvolution) {
    # un comment these lines to run deconvolution (SLOW!)  
    system(paste("Rscript detection/estGlobalV2.R", countryName), wait = TRUE)
    load(paste0("dat/",countryName,"/estDeconv.RData"))
  }

  # load dataList object
  load("dat/dataList.RData")

  if (verbose) {
    print('')
    print(paste('These', length(ddNames), 'regions now have data:'))
    print(ddNames)
  }
  
  # append data to dataList

  if (runDeconvolution) {
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
  } else {
    dataList[[countryName]] <- list(timeSeriesInfections = timeSeriesInfections,
                                     timeSeriesDeaths = timeSeriesDeaths,
                                     timeSeriesRecoveries = timeSeriesRecoveries,
                                     timeSeriesActive = timeSeriesActive,
                                     dates = dates,
                                     ddReg = ddReg,
                                     ddNames = ddNames,
                                     timestampRan = Sys.time())
  }

  # write datList back out
  save(dataList, file = "dat/dataList.RData")
  print("Complete")
} else {
  print('No data was saved')
}

}
