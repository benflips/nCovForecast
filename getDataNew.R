s## ---------------------------
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

library("readr")

## ---------------------------
## load up functions

source('functions.R')

## ---------------------------

## Get data
server <- FALSE ## if you are drawing data directly over internet, set this to FALSE to use url alternatives:
if (server){
  tsConf  <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" 
  tsDeath <- "/srv/shiny-server/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" 
} else {  
  tsConf  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  tsDeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
}

tsI<-read_csv(file = tsConf)
tsD<-read_csv(file = tsDeath)

## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols], format = "%m/%d/%y")

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])

## add recovery lag -- assumes all cases recover at 22 days
tsA <- recLag(tsI, tsD)


## GLOBAL

  tsICountry <- countryAgg(tsI) # aggregated to country
  tsACountry <- countryAgg(tsA) 
  tsDCountry <- countryAgg(tsD)

  ## Define menus
  # get region names with 20 or more cases as of yesterday
  ddNames <- tsACountry$Country[tsACountry[[ncol(tsACountry)-1]]>19]

  ddReg <- ddNames
  names(ddReg) <- ddNames

  ## run deconvolution to estimate undiagnosed cases
  source("detection/estGlobalV2.R")

  ## write data caches out
  save(ddReg, ddNames,                                   file =        "dat/Global/menuData.RData")
  save(tsI, tsD, tsA, tsACountry, dates, ddNames, ddReg, file = paste0("dat/Global/cacheData.RData"))

## AUSTRALIA

  focusCountry <- 'Australia'

  # subset to Australia
  tsI <- subset(tsI, tsI$Country.Region == focusCountry)
  tsD <- subset(tsD, tsD$Country.Region == focusCountry)

  tsA <- recLag(tsI, tsD)

  tsA <- natAgg(tsA)
  tsI <- natAgg(tsI)
  tsD <- natAgg(tsD)

  ## Define menus
  # get region names 
  ddNames      <- tsA$Province.State
  ddReg        <- ddNames
  names(ddReg) <- ddNames

  ## write data caches out
  save(ddReg, ddNames,                                   file =        "dat/Australia/menuData.RData")
  save(tsI, tsD, tsA, tsACountry, dates, ddNames, ddReg, file = paste0("dat/Australia/cacheData.RData"))
