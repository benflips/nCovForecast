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
## Copyright (c) Ben Phillips, 2020
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
source("functions.R")

## ---------------------------


## Get data
tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
tsDeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
tsRec <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

tsI<-read_csv(file = tsConf)
tsD<-read_csv(file = tsDeath)
tsR<-read_csv(file = tsRec)

## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols], format = "%m/%d/%y")

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
names(tsR)[!dCols] <- make.names(names(tsR)[!dCols])

## add recovery lag -- assumes confirmed cases are all tracked to recovery/death
matI<-as.matrix(tsI[, dCols])
matD<-as.matrix(tsD[, dCols])
matR<-as.matrix(tsR[, dCols])
matA<-matI - matD - matR

tsA <- cbind(tsI[,!dCols], matA) # active cases

tsACountry <- countryAgg(tsA) # aggregated to country
tsACountry <- tsACountry[rev(order(tsACountry[[ncol(tsACountry)-1]])),] # ordered from most to least active cases

## Define menus
# get region names with 20 or more cases as of yesterday
ddNames <- tsACountry$Country[tsACountry[[ncol(tsACountry)-1]]>29]
ddReg <- ddNames
names(ddReg) <- ddNames
#ddReg <- paste(ddReg, collapse = ", ") # menu specifier

#save(tsI, tsD, tsR, tsA, tsACountry, dates, ddNames, ddReg, file = paste0("dat/cacheData", format(Sys.Date(), format = "%m%d"), ".RData"))


