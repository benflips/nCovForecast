library(COVID19)
library(tidyverse)

createCSV <- function(countryName) {

updateCSV <- FALSE


filename = paste0('csvData/',countryName,'_confirmed.csv')

if (file.exists(filename)) {
  existingCSV <- read.csv(file = filename)
} else {
  message('File does not exist - create for first time')
  updateCSV <- TRUE
}

originalData <- covid19(c(countryName), level = 2, verbose=FALSE)
states <- unique(originalData$administrative_area_level_2)
dates <- unique(originalData$date)
seq <- c(1:length(dates))

if (!updateCSV) {

  mostRecentDateFromExistingCSV <- tail(names(existingCSV), n=1)
  mostRecentDateFromFreshData <- format(tail(dates, n=1), 'X%-m.%d.%y')

  if (mostRecentDateFromExistingCSV == mostRecentDateFromFreshData) {
    message('File exists but already up-to-date')
    updateCSV <- FALSE
  } else {
    message('File exists and data is out-of-date - updating...')
    updateCSV <- TRUE
  }
}

if (updateCSV) {

  dir.create("csvData", showWarnings = FALSE) # if the directory doesn't exist, create it.

  #confirmed
  dataConfirmed <- originalData %>% select(administrative_area_level_2, date, confirmed)
  outputConfirmed <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    outputConfirmed[[format(dates[value], '%-m/%d/%y')]] <- subset(dataConfirmed, dataConfirmed$date == dates[value])$confirmed
  }
  write.csv(outputConfirmed, paste0('csvData/',countryName, '_confirmed.csv'), quote=FALSE, row.names=FALSE)

  #deaths
  dataDeaths <- originalData %>% select(administrative_area_level_2, date, deaths)
  outputDeaths <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    outputDeaths[[format(dates[value], '%-m/%d/%y')]] <- subset(dataDeaths, dataDeaths$date == dates[value])$deaths
  }
  write.csv(outputDeaths, paste0('csvData/',countryName, '_deaths.csv'), quote=FALSE, row.names=FALSE)

  #recovered
  dataRecovered <- originalData %>% select(administrative_area_level_2, date, recovered)
  outputRecovered <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    outputRecovered[[format(dates[value], '%-m/%d/%y')]] <- subset(dataRecovered, dataRecovered$date == dates[value])$recovered
  }
  write.csv(outputRecovered, paste0('csvData/',countryName, '_recovered.csv'), quote=FALSE, row.names=FALSE)

}


}
