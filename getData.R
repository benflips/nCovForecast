## ---------------------------
##
## Script name: getData.R
##
## Purpose of script: Scrape data from gitHub repository established to track nCov20
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

source('functions.R')

## ---------------------------

## Get data
server <- FALSE ## if you are drawing data directly over internet, set this to FALSE to use url alternatives:
if (server){
  tsConf    <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  tsConfUS  <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  tsDeath   <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  tsDeathUS <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  tsRec     <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv" 
} else {  
  tsConf    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  tsConfUS  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  tsDeath   <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  tsDeathUS <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  tsRec     <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
}

timeSeriesInfections   <-loadData(tsConf)
timeSeriesInfectionsUS <-loadData(tsConfUS)
timeSeriesDeaths       <-loadData(tsDeath)
timeSeriesDeathsUS     <-loadData(tsDeathUS)
timeSeriesRecoveries   <-loadData(tsRec)

rm(tsConf, tsConfUS, tsDeath, tsDeathUS, tsRec) # tidy up

#aggregate US data to Province.State
timeSeriesInfectionsUS <-regionAgg(timeSeriesInfectionsUS, regionCol = timeSeriesInfectionsUS$Province.State, regionName = "Province.State")
  timeSeriesInfectionsUS$Country.Region <- rep("US", nrow(timeSeriesInfectionsUS))
  timeSeriesInfectionsUS <- timeSeriesInfectionsUS[c(ncol(timeSeriesInfectionsUS), 1:(ncol(timeSeriesInfectionsUS)-1))] 
timeSeriesDeathsUS <-regionAgg(timeSeriesDeathsUS, regionCol = timeSeriesDeathsUS$Province.State, regionName = "Province.State")
  timeSeriesDeathsUS$Country.Region <- rep("US", nrow(timeSeriesDeathsUS))
  timeSeriesDeathsUS <- timeSeriesDeathsUS[c(ncol(timeSeriesDeathsUS), 1:(ncol(timeSeriesDeathsUS)-1))] 

# Test for structural irregularities
  # US and global data are up to the same date
test1 <- ncol(timeSeriesDeaths)==ncol(timeSeriesDeathsUS) & ncol(timeSeriesInfections)==ncol(timeSeriesInfectionsUS) 
  # Infection and death data have same number of rows
test2 <- nrow(timeSeriesDeathsUS)==nrow(timeSeriesInfectionsUS) & nrow(timeSeriesDeaths)==nrow(timeSeriesInfections)
  # NAs anywhere in the data
test3 <- (sum(is.na(timeSeriesInfections))+sum(is.na(timeSeriesDeaths))+sum(is.na(timeSeriesRecoveries))+sum(is.na(timeSeriesInfectionsUS))+sum(is.na(timeSeriesDeathsUS)))==0

if (test1 & test2 & test3){
  # Merge US data with global dataframes
  timeSeriesInfections <- rbind(subset(timeSeriesInfections, timeSeriesInfections$Country.Region!="US") , timeSeriesInfectionsUS)
  timeSeriesDeaths <- rbind(subset(timeSeriesDeaths, timeSeriesDeaths$Country.Region!="US") , timeSeriesDeathsUS)
  
  rm(timeSeriesDeathsUS, timeSeriesInfectionsUS) #tidy up
  
  # a check
  #sum(!(table(timeSeriesDeaths$Country.Region, timeSeriesDeaths$Province.State) == table(timeSeriesInfections$Country.Region, timeSeriesInfections$Province.State)))
  
  # take US, Canada data, and generate recovery data assuming ttr
  infSub   <- subset(timeSeriesInfections, timeSeriesInfections$Country.Region %in% c("Canada", "US"))
  deathSub <- subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     %in% c("Canada", "US"))
  recSub   <- recLag(infSub, deathSub, active = FALSE)
  
  # Merge US, Canada estimated recoveries on to known recoveries
  timeSeriesRecoveries <- rbind(subset(timeSeriesRecoveries, !(timeSeriesRecoveries$Country.Region %in% c("US", "Canada"))) , recSub)
  
  # a check
  #sum(!(table(timeSeriesRecoveries$Country.Region) == table(timeSeriesInfections$Country.Region)))
  rm(infSub, deathSub, recSub) # tidy up
  
  
  ## standardise
  
  ## get Date range
  dCols<-dateCols(timeSeriesInfections)
  dates<-as.Date(colnames(timeSeriesInfections)[dCols], format = "%m.%d.%y")
  
  
  # Standardise dataframes and compute active cases
  std <- activeCases(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries)
  
  
  # Create a list to hold all data
  available_countries <- c("Australia","China", "Canada", "US") # countries available for drill-down
  dataList <- vector(mode = "list", length = length(available_countries)+1)
  names(dataList) <- c("Global", available_countries)
  
  
  ###### GLOBAL ######
  
  timeSeriesInfections <- regionAgg(std$tsI, regionCol = std$tsI$Country.Region, regionName = "Region") # aggregated to country
  timeSeriesDeaths     <- regionAgg(std$tsD, regionCol = std$tsD$Country.Region, regionName = "Region") 
  timeSeriesRecoveries <- regionAgg(std$tsR, regionCol = std$tsR$Country.Region, regionName = "Region")
  timeSeriesActive     <- regionAgg(std$tsA, regionCol = std$tsA$Country.Region, regionName = "Region")
  
  ## Define menus
  # get region names with 20 or more cases as of yesterday
  ddNames <- timeSeriesActive$Region[timeSeriesActive[[ncol(timeSeriesActive)-1]]>19]
  
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
                          active.cases = active.cases)
  
  ###### LOCAL ######
  
  for(focusCountry in available_countries) {
    
    print(focusCountry)
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
    # get region names 
    ddNames      <- timeSeriesActive$Region
    ddReg        <- ddNames
    names(ddReg) <- ddNames
    
    ## write data caches out
    dir.create(paste0("dat/", focusCountry), showWarnings = FALSE) # if the directory doesn't exist, create it.
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
                                     active.cases = active.cases)
  }
  
  save(dataList, file = "dat/dataList.RData")
  
} else { print('there was an error!') } # end of first data test if statement (test1, test2) ...need to add our else notification here
