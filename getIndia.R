## ---------------------------
##
## Script name: getIndia.R
##
## Purpose of script: to get India data from Vipin/Jyoti's repo, independently from JHU process
##
## Author: Ben Phillips
##
## Date Created: 2020-05-04
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

## load up our functions into memory
source('functions.R')

## ---------------------------
cat("Loading India data...\n\n")

focusCountry <- "India" # define country string

tsConfIndia  <- "https://raw.githubusercontent.com/vipinbhatnagar/covid19/master/confirmed.csv"
tsDeathIndia <- "https://raw.githubusercontent.com/vipinbhatnagar/covid19/master/deaths.csv"


timeSeriesInfections <-loadData(tsConfIndia)
timeSeriesDeaths     <-loadData(tsDeathIndia)

rm(tsConfIndia, tsDeathIndia)

# test structural integrity of data
test1 <- nrow(timeSeriesDeaths)==nrow(timeSeriesInfections)
test2 <- ncol(timeSeriesDeaths)==ncol(timeSeriesInfections)
# NAs anywhere in the data
test3 <- (sum(is.na(timeSeriesInfections))+sum(is.na(timeSeriesDeaths)))==0

cat(paste("Death and infection data equal nrows:", test1, "\n"))
cat(paste("Death and infection data equal ncols:", test2, "\n"))
cat(paste("No NAs anywhere in the data:", test3, "\n\n"))

if (test1 & test2 & test3) {
  
  ## get Date range 
  dCols<-dateCols(timeSeriesInfections)
  dates<-as.Date(colnames(timeSeriesInfections)[dCols], format = "%m.%d.%y")
  
  ## generate recovery data
  timeSeriesRecoveries   <- recLag(timeSeriesInfections, timeSeriesDeaths, active = FALSE)
  
  
  # Standardise dataframes and compute active cases
  std <- activeCases(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries)
  
  # report to console countries that have been recLagged within activeCases()
  cat("States failing recovery data test and treated with recLag: ", nrow(std$failedRecovery), "\n", paste(std$failedRecovery[,1], std$failedRecovery[,2], "\n"), "\n\n")
  
  # exclude data where there are large errors in the infection and death cumulants
  checkI <- cumulantCheck(std$tsI)
  checkD <- cumulantCheck(std$tsD)
  cumSub <- checkI & checkD
  if (sum(!cumSub)>5) stop("More than five suspect regions in India dataset.")
  cat(paste("States excluded through failed cumulants:", sum(!cumSub), "\n"))
  print(cbind(std$tsI[!cumSub, 1:2], checkI = checkI[!cumSub], checkD = checkD[!cumSub]))
  cat("\n\n")
  tsI <- std$tsI[cumSub,]
  tsD <- std$tsD[cumSub,]
  tsR <- std$tsR[cumSub,]
  tsA <- std$tsA[cumSub,]
  rm(checkI, checkD, cumSub)
  
  cat("Organizing data for India...\n")
  
  # aggregate to region
  tsI <- regionAgg(tsI, regionCol = tsI$Province.State)
  tsD <- regionAgg(tsD, regionCol = tsD$Province.State)
  tsR <- regionAgg(tsR, regionCol = tsR$Province.State)
  tsA <- regionAgg(tsA, regionCol = tsA$Province.State)
  
  timeSeriesInfections <- natAgg(tsI, aggName = paste("National aggregate -", focusCountry))
  timeSeriesDeaths <- natAgg(tsD, aggName = paste("National aggregate -", focusCountry))
  timeSeriesRecoveries <- natAgg(tsR, aggName = paste("National aggregate -", focusCountry))
  timeSeriesActive <- natAgg(tsA, aggName = paste("National aggregate -", focusCountry))
  
  ## Define menus
  # get region names with 20 or more cases as of yesterday
  ddNames <- timeSeriesInfections$Region[timeSeriesInfections[[ncol(timeSeriesInfections)-1]]>19]
  ddReg        <- ddNames
  names(ddReg) <- ddNames
  
  ## write data caches out
  dir.create(paste0("dat/", focusCountry), showWarnings = FALSE) # if the directory doesn't exist, create it.
  save(ddReg, ddNames, file = paste0("dat/",focusCountry,"/menuData.RData"))
  save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = paste0("dat/",focusCountry,"/cacheData.RData"))
  
  system(paste("Rscript detection/estGlobalV2.R", focusCountry), wait = TRUE)
  load(paste0("dat/",focusCountry,"/estDeconv.RData"))
  
  # load dataList object
  load("dat/dataList.RData")
  
  # append India data
  dataList[[focusCountry]] <- list(timeSeriesInfections = timeSeriesInfections,
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
  cat("getData complete.\n")  
} else { stop(paste('there was an error!', test1, test2, test3)) }  