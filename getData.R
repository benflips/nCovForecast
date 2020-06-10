## ---------------------------
##
## Script name: getData.R
##
## Purpose of script: 
## 1. Get data from JHU;
## 2. Build directory structure for all state-level data;
## 3. If dataList doesn't already exist, create it
## 4. Build global dataset and state-level for countries available in JHU dataset
## 5. If dataList doesn't exist run retrieval of state-level data for other (non-JHU) datasets.
##
## Author: Ben Phillips
##
## Date Created: 2020-02-07
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need:  


## ---------------------------
## load up functions

#source('getDataGeneral.R')

#getDataGeneral('Global',
#               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
#               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
#               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
#               FALSE)

getDataGeneral('US',
               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv', 
               '',
               TRUE,FALSE)

getDataGeneral('Canada',
               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
               'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv','',
               FALSE,TRUE)


source('functions.R')

## ---------------------------

## Get data
  tsConf       <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  tsDeath      <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  tsRec        <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

cat("Loading JHU data...\n\n")
timeSeriesInfections      <-loadData(tsConf)
timeSeriesDeaths          <-loadData(tsDeath)
timeSeriesRecoveries      <-loadData(tsRec)


## get Date range from JHU
dCols<-dateCols(timeSeriesInfections)
dates<-as.Date(colnames(timeSeriesInfections)[dCols], format = "%m.%d.%y")


rm(tsConf, tsDeath,  tsRec) # tidy up


# Test for structural irregularities in data before proceeding any further
  # US and global data are up to the same date
test1 <- ncol(timeSeriesDeaths)==ncol(timeSeriesDeathsUS) & ncol(timeSeriesInfections)==ncol(timeSeriesInfectionsUS) 
  # Infection and death data have same number of rows
test2 <- nrow(timeSeriesDeathsUS)==nrow(timeSeriesInfectionsUS) & nrow(timeSeriesDeaths)==nrow(timeSeriesInfections)
  # NAs anywhere in the data
test3 <- (sum(is.na(timeSeriesInfections))+sum(is.na(timeSeriesDeaths))+sum(is.na(timeSeriesRecoveries))+sum(is.na(timeSeriesInfectionsUS))+sum(is.na(timeSeriesDeathsUS)))==0

cat(paste("US and Global dates align:", test1, "\n"))
cat(paste("Infection and death equal nrows:", test2, "\n"))
cat(paste("No NAs anywhere in the data:", test3, "\n\n"))

if (test1 & test2 & test3){

  # Merge US data with global dataframes
  timeSeriesInfections <- rbind(subset(timeSeriesInfections, timeSeriesInfections$Country.Region != "US"), timeSeriesInfectionsUS)
  timeSeriesDeaths     <- rbind(subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     != "US"), timeSeriesDeathsUS)

  
  rm(timeSeriesDeathsUS, timeSeriesInfectionsUS) #tidy up
  
  # a check
  #sum(!(table(timeSeriesDeaths$Country.Region, timeSeriesDeaths$Province.State) == table(timeSeriesInfections$Country.Region, timeSeriesInfections$Province.State)))
  
  # take US, Canada and generate recovery data assuming ttr
  recMissing <- c("Canada", "US") # countries for which recovery data are missing
  infSub   <- subset(timeSeriesInfections, timeSeriesInfections$Country.Region %in% recMissing)
  deathSub <- subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     %in% recMissing)
  recSub   <- recLag(infSub, deathSub, active = FALSE)
  # Merge US, Canada estimated recoveries on to known recoveries
  timeSeriesRecoveries <- rbind(subset(timeSeriesRecoveries, !(timeSeriesRecoveries$Country.Region %in% recMissing)) , recSub)
  
  # a check
  #sum(!(table(timeSeriesRecoveries$Country.Region) == table(timeSeriesInfections$Country.Region)))
  rm(infSub, deathSub, recSub) # tidy up
  
  ## standardise
  # Standardise dataframes and compute active cases
  std <- activeCases(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries)
  
  # report to console countries that have been recLagged within activeCases()
  cat("Countries failing recovery data test and treated with recLag: ", nrow(std$failedRecovery), "\n", paste(std$failedRecovery[,1], std$failedRecovery[,2], "\n"), "\n\n")
  
  # exclude data where there are large errors in the infection death and recovery cumulants
  checkI <- cumulantCheck(std$tsI)
  checkD <- cumulantCheck(std$tsD)
  checkR <- cumulantCheck(std$tsR, tolerance = 0.5)
  cumSub <- checkI & checkD & checkR
  if (sum(!cumSub)>10) stop("More than ten suspect regions in JHU dataset.")
  cat(paste("Countries excluded through failed cumulants:", sum(!cumSub), "\n"))
  print(cbind(std$tsI[!cumSub, 1:2], checkI = checkI[!cumSub], checkD = checkD[!cumSub], checkR = checkR[!cumSub]))
  cat("\n\n")
  std$tsI <- std$tsI[cumSub,]
  std$tsD <- std$tsD[cumSub,]
  std$tsR <- std$tsR[cumSub,]
  std$tsA <- std$tsA[cumSub,]
  rm(checkI, checkD, checkR, cumSub)

  
  
  
  # Open dataList object, or create it
  available_countries <- c("Australia","China", "Canada", "US", "India") # countries available for drill-down
  if (file.exists("dat/dataList.RData")) {
    load("dat/dataList.RData")
    runSeparately <- TRUE # if dataList exists, run country updates separately
  } else {
    dataList <- vector(mode = "list", length = length(available_countries)+1)
    names(dataList) <- c("Global", available_countries)
    runSeparately <- FALSE # if dataList exists, run country updates separately
  }
  
  
  
  ###### GLOBAL ######
  cat("Organizing data for...\n")
  cat("Global\n") # report to console
  
  timeSeriesInfections <- regionAgg(std$tsI, regionCol = std$tsI$Country.Region, regionName = "Region") # aggregated to country
  timeSeriesDeaths     <- regionAgg(std$tsD, regionCol = std$tsD$Country.Region, regionName = "Region") 
  timeSeriesRecoveries <- regionAgg(std$tsR, regionCol = std$tsR$Country.Region, regionName = "Region")
  timeSeriesActive     <- regionAgg(std$tsA, regionCol = std$tsA$Country.Region, regionName = "Region")
  
  # create global aggregate row
  timeSeriesInfections <- natAgg(timeSeriesInfections, aggName = "Global aggregate")
  timeSeriesDeaths     <- natAgg(timeSeriesDeaths,     aggName = "Global aggregate")
  timeSeriesRecoveries <- natAgg(timeSeriesRecoveries, aggName = "Global aggregate")
  timeSeriesActive     <- natAgg(timeSeriesActive,     aggName = "Global aggregate")
  
  # Make continent aggregates
  load("dat/Continents/continentData.RData")
  timeSeriesInfections <- continentAgg(timeSeriesInfections, continentData)
  timeSeriesDeaths <- continentAgg(timeSeriesDeaths, continentData)
  timeSeriesRecoveries <- continentAgg(timeSeriesRecoveries, continentData)
  timeSeriesActive <- continentAgg(timeSeriesActive, continentData)
  
  ## Define menus
  # get region names with 20 or more cases as of yesterday
  ddNames <- timeSeriesInfections$Region[timeSeriesInfections[[ncol(timeSeriesInfections)-1]]>19]
  
  ddReg <- ddNames
  names(ddReg) <- ddNames
  
  ## write data caches out
  dir.create("dat/Global", recursive = TRUE, showWarnings = FALSE) # if the directory doesn't exist, create it.
  save(ddReg, ddNames, dates, file = "dat/Global/menuData.RData")
  save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = "dat/Global/cacheData.RData")
  
  ## run deconvolution to estimate undiagnosed cases from cached data
  system("Rscript detection/estGlobalV2.R 'Global'", wait = TRUE)
  load("dat/Global/estDeconv.RData")
  
  dataList$Global <- list(timeSeriesInfections = timeSeriesInfections,
                          timeSeriesDeaths = timeSeriesDeaths,
                          timeSeriesRecoveries = timeSeriesRecoveries,
                          timeSeriesActive = timeSeriesActive,
                          dates = dates,
                          ddReg = ddReg,
                          ddNames = ddNames,
                          cumulative.infections = cumulative.infections,
                          undiagnosed.infections = undiagnosed.infections, 
                          active.projections = active.projections)
  
  ###### LOCAL ######
  
  for(focusCountry in available_countries) {
    # check country directory exists
    dir.create(paste0("dat/", focusCountry), showWarnings = FALSE) # if the directory doesn't exist, create it.
    
    if (focusCountry=="India") next # do India independently, in getIndia.R
    cat(focusCountry, "\n")
    
    # set dataframes back to standards
    tsI <- std$tsI
    tsD <- std$tsD
    tsR <- std$tsR
    tsA <- std$tsA
    
    # subset to focusCountry
    tsI <- subset(tsI, tsI$Country.Region == focusCountry)
    tsD <- subset(tsD, tsD$Country.Region == focusCountry)
    tsR <- subset(tsR, tsR$Country.Region == focusCountry)
    tsA <- subset(tsA, tsA$Country.Region == focusCountry)
    
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
    save(ddReg, ddNames, file = paste0("dat/",focusCountry,"/menuData.RData"))
    save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = paste0("dat/",focusCountry,"/cacheData.RData"))
    
    system(paste("Rscript detection/estGlobalV2.R", focusCountry), wait = TRUE)
    load(paste0("dat/",focusCountry,"/estDeconv.RData"))
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
  }
  
  save(dataList, file = "dat/dataList.RData")
  
  # run other countries
  if (!runSeparately){
    system("Rscript getIndia.R", wait = TRUE)
  }
cat("getData complete.\n")  
} else { stop(paste('there was an error!', test1, test2, test3)) } # end of first data test if statement (test1, test2) ...need to add our else notification here
