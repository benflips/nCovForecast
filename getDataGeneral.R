runDeconvolution <- function(countryName, deconvProcess = 1) {

  t1 = Sys.time()

  if (deconvProcess == 1){
    system(paste("Rscript detection/estGlobalV2.R", countryName), wait = TRUE) # old process (faster)
  } else {
    system(paste("Rscript detection/deconvEst.R", countryName), wait = TRUE) # new process (slower, but better)
  }

  load(paste0("dat/",countryName,"/estDeconv.RData"))

  # load dataList object
  load("dat/dataList.RData")

  # append data to dataList
  dataList[[countryName]]$cumulative.infections <- cumulative.infections
  dataList[[countryName]]$undiagnosed.infections <- undiagnosed.infections

  # write datList back out
  save(dataList, file = "dat/dataList.RData")

  t2 = Sys.time()
  print(t2-t1)

}


getDataCovid19datahub <- function(countryName) {
  getDataGeneral(countryName, paste0('./csvData/',countryName,'_confirmed.csv'),
                                                     paste0('./csvData/',countryName,'_deaths.csv'),
                                                     paste0('./csvData/',countryName,'_recovered.csv'), TRUE)
}

getDataGeneral <- function(countryName, inputConfirmed, inputDeaths, inputRecovered, verbose){


t1 = Sys.time()

source('functions.R')

inputRecoveredSupplied <- (inputRecovered != '')


noErrors <- TRUE

timeSeriesInfections <-loadData(inputConfirmed)
timeSeriesDeaths     <-loadData(inputDeaths)
if (inputRecoveredSupplied) {
  timeSeriesRecoveries <- loadData(inputRecovered)
}

if (verbose) {
  message(paste0(countryName, ': ', format(strptime(tail(names(timeSeriesInfections), n=1),'%m.%d.%y'), '%d %B %Y')))
}

printVerbose('Initially...',timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose)

if (countryName == 'Global') {
  #if (verbose) {
  #  print('The main reason for difference here is that Canada has many lines in infections, but only one line in recoveries')
  #}


  countriesToGenerateWithRecLag <- c('United Kingdom','Sweden','Netherlands','Serbia','Honduras','Namibia','Belgium','Central African Republic','Equatorial Guinea',
                                     'Greece','France','Egypt','US','Guinea-Bissau','Somalia','Mozambique','South Sudan','Kyrgyzstan','Libya','Bolivia','Congo (Brazzaville)',
                                     'Nigeria','Ecuador','Kazakhstan','Sudan','Philippines','Madagascar','Congo (Kinshasa)','Benin','Spain','Haiti','Guatemala','Gabon',
                                     'Ghana','South Africa','Suriname','Ukraine','Iraq','Afghanistan','Czechia','Liberia','Mexico','India','Canada')


  timeSeriesRecoveries <- subset(timeSeriesRecoveries, !(timeSeriesRecoveries$Country.Region %in% countriesToGenerateWithRecLag))
  timeSeriesRecoveries <- rbind(timeSeriesRecoveries, recLag(subset(timeSeriesInfections, timeSeriesInfections$Country.Region %in% countriesToGenerateWithRecLag),
                                                             subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     %in% countriesToGenerateWithRecLag), active=FALSE))

  timeSeriesInfections$Province.State <- NULL
  timeSeriesDeaths$Province.State     <- NULL
  timeSeriesRecoveries$Province.State <- NULL

  #printVerbose(paste0('After excluding countries: (',toString(countriesToGenerateWithRecLag),') with known bad recovery data and using recLag to generate those instead'),timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose)

} else {

  timeSeriesInfections <- subset(timeSeriesInfections, timeSeriesInfections$Country.Region == countryName)
  timeSeriesDeaths     <- subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     == countryName)
  timeSeriesInfections$Country.Region <- NULL
  timeSeriesDeaths$Country.Region     <- NULL
  if (inputRecoveredSupplied) {
    timeSeriesRecoveries <- subset(timeSeriesRecoveries, timeSeriesRecoveries$Country.Region == countryName)
    timeSeriesRecoveries$Country.Region <- NULL
  }

  #printVerbose('After limiting to the focus country', timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose)
}


names(timeSeriesInfections)[1] <- 'Region'
names(timeSeriesDeaths)[1]     <- 'Region'
if (inputRecoveredSupplied) {
  names(timeSeriesRecoveries)[1] <- 'Region'
}
#printVerbose('Should always have one non-date column, and it should be simply called Region', timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose)

rm(inputConfirmed, inputDeaths, inputRecovered)

# always aggregate over region column - sometimes this will do things, sometimes not.
timeSeriesInfections <- regionAggregate(timeSeriesInfections)
timeSeriesDeaths     <- regionAggregate(timeSeriesDeaths)
if (inputRecoveredSupplied) {
  timeSeriesRecoveries <- regionAggregate(timeSeriesRecoveries)
}

#printVerbose('After aggregating over Region column', timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose)



# test structural integrity of data
test1 <- checkSameNumberOfRows(timeSeriesInfections, timeSeriesDeaths)
test2 <- checkSameNumberOfCols(timeSeriesInfections, timeSeriesDeaths)

test3 <- checkNAs(timeSeriesInfections)
test4 <- checkNAs(timeSeriesDeaths)

noErrors <- noErrors & test1 & test2 & test3 & test4

if (inputRecoveredSupplied) {

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
  if (!inputRecoveredSupplied) {
    timeSeriesRecoveries <- recLag(timeSeriesInfections, timeSeriesDeaths, active = FALSE)
  }

  # Standardise dataframes and compute active cases
  std <- activeCases(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries)
  
  #printVerbose('Function activeCases complete', timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose)

  # report to console countries that have been recLagged within activeCases()
  if (nrow(std$failedRecovery)>0 & verbose) {
    message('')
    message(paste("There are", nrow(std$failedRecovery), "region(s) failing recovery data test and treated with recLag: "))
    print(std$failedRecovery)
    cat("\n\n")
  }

  # exclude data where there are large errors in the infection and death cumulants
  checkI <- cumulantCheck(std$tsI)
  checkD <- cumulantCheck(std$tsD)
  checkR <- cumulantCheck(std$tsR, tolerance = 0.5)

  cumSub <- checkI & checkD & checkR
  if (sum(!cumSub)>10) stop("More than five suspect regions in this dataset.")
  if (sum(!cumSub)>0 & verbose) {
    message(paste("These regions will be completely excluded, due to failed cumulants:", sum(!cumSub)))
    print(cbind(std$tsI[!cumSub, 1:2], checkI = checkI[!cumSub], checkD = checkD[!cumSub], checkR = checkR[!cumSub]))
    cat("\n\n")
  }
  timeSeriesInfections <- std$tsI[cumSub,]
  timeSeriesDeaths     <- std$tsD[cumSub,]
  timeSeriesRecoveries <- std$tsR[cumSub,]
  timeSeriesActive     <- std$tsA[cumSub,]
  rm(checkI, checkD, checkR, cumSub)

  #printVerbose('After excluding regions which failed cumulants', timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, TRUE, verbose)
  printVerbose('Finally...', timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, TRUE, verbose)

  if (countryName == 'Global') {
    # create global aggregate row
    timeSeriesInfections <- natAgg(timeSeriesInfections, aggName = "Global aggregate")
    timeSeriesDeaths     <- natAgg(timeSeriesDeaths,     aggName = "Global aggregate")
    timeSeriesRecoveries <- natAgg(timeSeriesRecoveries, aggName = "Global aggregate")
    timeSeriesActive     <- natAgg(timeSeriesActive,     aggName = "Global aggregate")

    # Make continent aggregates
    load("dat/Continents/continentData.RData")
    timeSeriesInfections <- continentAgg(timeSeriesInfections, continentData)
    timeSeriesDeaths     <- continentAgg(timeSeriesDeaths,     continentData)
    timeSeriesRecoveries <- continentAgg(timeSeriesRecoveries, continentData)
    timeSeriesActive     <- continentAgg(timeSeriesActive,     continentData)

  } else {
    timeSeriesInfections <- natAgg(timeSeriesInfections, aggName = paste("National aggregate -", countryName))
    timeSeriesDeaths     <- natAgg(timeSeriesDeaths,     aggName = paste("National aggregate -", countryName))
    timeSeriesRecoveries <- natAgg(timeSeriesRecoveries, aggName = paste("National aggregate -", countryName))
    timeSeriesActive     <- natAgg(timeSeriesActive,     aggName = paste("National aggregate -", countryName))
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
  
  load(paste0("dat/",countryName,"/estDeconv.RData"))

  # load dataList object
  load("dat/dataList.RData")

  if (verbose) {
    message('')
    message(paste('These', length(ddNames), 'regions now have data:'))
    print(ddNames)
  }
  
  # append data to dataList

  dataList[[countryName]] <- list(timeSeriesInfections   = timeSeriesInfections,
                                  timeSeriesDeaths       = timeSeriesDeaths,
                                  timeSeriesRecoveries   = timeSeriesRecoveries,
                                  timeSeriesActive       = timeSeriesActive,
                                  dates                  = dates,
                                  ddReg                  = ddReg,
                                  ddNames                = ddNames,
                                  cumulative.infections  = cumulative.infections,
                                  undiagnosed.infections = undiagnosed.infections)

  # write datList back out
  save(dataList, file = "dat/dataList.RData")
  if (verbose) {
    message('')
    message("Complete")
  }
} else {
  if (verbose) {
    message('No data was saved')
  }
}

t2 = Sys.time()
if (verbose) { 
  print(t2 - t1)
}

}


printVerbose <- function(initialMessage,timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, inputRecoveredSupplied, verbose) {
  if (verbose) {
    message('')
    message('----------------------------------')
    message(initialMessage)
    print(names(timeSeriesInfections)[1:4])
    message(paste(toString(dim(timeSeriesInfections)), '- dimensions of infections'))
    message(paste(toString(dim(timeSeriesDeaths)),     '- dimensions of deaths'))
    if (inputRecoveredSupplied) {
      message(paste(toString(dim(timeSeriesRecoveries)), '- dimensions of recoveries'))
    }
  }
}
