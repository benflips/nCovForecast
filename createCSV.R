library(COVID19)
library(tidyverse)

createCSV <- function(countryName, libraryToSourceFrom = 'COVID19') {


updateCSV <- FALSE

filename = paste0('csvData/',countryName,'_confirmed.csv')

if (file.exists(filename)) {
  existingCSV <- read.csv(file = filename)
} else {
  message('File does not exist - create for first time')
  updateCSV <- TRUE
}

if (libraryToSourceFrom == 'COVID19') { # covid19datahub.io
  library(COVID19)
  originalData <- covid19(c(countryName), level = 2, verbose=FALSE)
  states <- unique(originalData$administrative_area_level_2)
  originalData <- originalData %>% rename(state           = administrative_area_level_2,
                                          cases_total     = confirmed,
                                          deaths_total    = deaths,
                                          recovered_total = recovered) # rename columns from COVID19 to have same format as covidregionaldata
} else if (libraryToSourceFrom == 'covidregionaldata') { # https://cran.r-project.org/web/packages/covidregionaldata/
  library(covidregionaldata)
  originalData <- get_regional_data(countryName)
  if (countryName == 'Russia') {
    originalData <- originalData %>% rename(state = region)
  }
  states <- unique(originalData$state)
  originalData[is.na(originalData)] <- 0 # replace NAs with 0
}

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
  dataConfirmed <- originalData %>% select(state, date, cases_total)
  outputConfirmed <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    outputConfirmed[[format(dates[value], '%-m/%d/%y')]] <- subset(dataConfirmed, dataConfirmed$date == dates[value])$cases_total
  }
  write.csv(outputConfirmed, paste0('csvData/',countryName, '_confirmed.csv'), quote=FALSE, row.names=FALSE)

  #deaths
  dataDeaths <- originalData %>% select(state, date, deaths_total)
  outputDeaths <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    outputDeaths[[format(dates[value], '%-m/%d/%y')]] <- subset(dataDeaths, dataDeaths$date == dates[value])$deaths_total
  }
  write.csv(outputDeaths, paste0('csvData/',countryName, '_deaths.csv'), quote=FALSE, row.names=FALSE)

  #recovered
  dataRecovered <- originalData %>% select(state, date, recovered_total)
  outputRecovered <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    outputRecovered[[format(dates[value], '%-m/%d/%y')]] <- subset(dataRecovered, dataRecovered$date == dates[value])$recovered_total
  }
  write.csv(outputRecovered, paste0('csvData/',countryName, '_recovered.csv'), quote=FALSE, row.names=FALSE)

}


}
