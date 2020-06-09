getDataGeneral <- function(countryName, inputConfirmed, inputDeaths, aggregateOverProvinceState){
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


timeSeriesInfections <-loadData(inputConfirmed)
timeSeriesDeaths     <-loadData(inputDeaths)

rm(inputConfirmed, inputDeaths)

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
test1 <- nrow(timeSeriesDeaths)==nrow(timeSeriesInfections)
if (!test1) {
  print("Death and infection data DO NOT have an equal number of rows\n")
}

test2 <- ncol(timeSeriesDeaths)==ncol(timeSeriesInfections)
if (!test2) {
  print("Death and infection data DO NOT have an equal number of columns\n")
}

# NAs anywhere in the data
test3 <- sum(is.na(timeSeriesInfections))==0
if (!test3) {
  print("There are NAs somewhere in the infection data\n")
}

test4 <- sum(is.na(timeSeriesDeaths))==0
if (!test4) {
  print("There are NAs somewhere in the death data\n")
}


if (test1 & test2 & test3 & test4) {

  # All tests passed successfully
  
  ## get date range 
  dCols<-dateCols(timeSeriesInfections)
  dates<-as.Date(colnames(timeSeriesInfections)[dCols], format = "%m.%d.%y")
  
  ## generate recovery data
  timeSeriesRecoveries   <- recLag(timeSeriesInfections, timeSeriesDeaths, active = FALSE)
  
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
    print(paste("States excluded through failed cumulants:", sum(!cumSub), "\n"))
    print(cbind(std$tsI[!cumSub, 1:2], checkI = checkI[!cumSub], checkD = checkD[!cumSub]))
    print("\n\n")
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
  
  timeSeriesInfections <- natAgg(tsI, aggName = paste("National aggregate -", countryName))
  timeSeriesDeaths     <- natAgg(tsD, aggName = paste("National aggregate -", countryName))
  timeSeriesRecoveries <- natAgg(tsR, aggName = paste("National aggregate -", countryName))
  timeSeriesActive     <- natAgg(tsA, aggName = paste("National aggregate -", countryName))
  
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
                                   active.projections = active.projections)
  
  # write datList back out
  save(dataList, file = "dat/dataList.RData")
  print("Complete")
} else { stop(paste('there was an error!', test1, test2, test3, test4)) }  

}
