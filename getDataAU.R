## ---------------------------
##
## Script name: getDataAU
##
## Purpose of script: Gather data from Guardian dataset 
##                      Here: https://www.theguardian.com/australia-news/datablog/ng-interactive/2020/apr/24/coronavirus-australia-numbers-how-many-new-cases-today-maps-deaths-death-toll-covid-19-stats-statistics-graph-map-by-postcode
##
## Author: Ben Phillips
##
## Date Created: 2020-04-26
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes: This is called from getData and replaces JHU recoveries in Australia with more accurate data from the Guardian
##   
##
## --------------------------
## load up the packages we will need 
library("gsheet")
## ---------------------------

## load up our functions into memory

## ---------------------------

# get data
baseURL <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"
d <- gsheet2tbl(baseURL)

# format dates
d$Date <- as.Date(d$Date, format = "%d/%m/%Y")
# throw out columns we don't need
d <- d[, c("State", "Date", "Cumulative case count", "Cumulative deaths", "Recovered (cumulative)")]
d <- subset(d, d$Date<=max(dates))
# enforce dates as per JHU dataset -- dates object comes from getData.R
d$Date <- factor(d$Date, levels = levels(factor(dates))) 

# empty variables to take results
auI <- c()
auD <- c()
auR <- c()

# enforces data into a cumulant -- fills NAs and corrects any (non-cumulant) errors
makeCumulant <- function(x){
  x[1]<-0
  # push through NAs
  for (dd in 2: length(x)){
    if (is.na(x[dd])) x[dd]<-x[dd-1]
  }
  # catch errors in cumulative data
  for (dd in 2: length(x)){
    if (!(is.na(x[dd]) | is.na(x[dd-1]))){
      if ((x[dd]<x[dd-1])) x[x==x[dd-1]]<-NA
    }
  }
  # push through NAs again
  for (dd in 2: length(x)){
    if (is.na(x[dd])) x[dd]<-x[dd-1]
  }
  x
}

# step through states and build JHU-structured dataframes.
for (ss in unique(d$State)){
  dSub <- subset(d, d$State==ss)
  # Infections
  tsI <- tapply(dSub$`Cumulative case count`, INDEX = list(date = dSub$Date), FUN = sum)
  tsI <- makeCumulant(tsI)
  auI <- rbind(auI, tsI)
  # Deaths
  tsD <- tapply(dSub$`Cumulative deaths`, INDEX = list(date = dSub$Date), FUN = sum)
  tsD <- makeCumulant(tsD)
  auD <- rbind(auD, tsD)
  # Recoveries
  tsR <- tapply(dSub$`Recovered (cumulative)`, INDEX = list(date = dSub$Date), FUN = sum)
  tsR <- makeCumulant(tsR)
  auR <- rbind(auR, tsR)
}
Province.State <- matrix(unique(d$State), nrow = length(unique(d$State)))
Country.Region <- matrix("Australia", nrow = length(unique(d$State)), ncol = 1)
auI <- data.frame(Country.Region, Province.State, auI, check.names = FALSE)
auD <- data.frame(Country.Region, Province.State, auD, check.names = FALSE)
auR <- data.frame(Country.Region, Province.State, auR, check.names = FALSE)

names(auI) <- names(timeSeriesInfections)
names(auD) <- names(timeSeriesInfections)
names(auR) <- names(timeSeriesInfections)

# swap out JHU with Guardian data for Australia
timeSeriesInfections <- rbind(subset(timeSeriesInfections, timeSeriesInfections$Country.Region!="Australia") , auI)
timeSeriesDeaths <- rbind(subset(timeSeriesDeaths, timeSeriesDeaths$Country.Region!="Australia") , auR)
timeSeriesRecoveries <- rbind(subset(timeSeriesRecoveries, timeSeriesRecoveries$Country.Region!="Australia") , auR)


