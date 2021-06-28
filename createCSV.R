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
  #Fill in missing/NA values
  if (sum(is.na(originalData$cases_total)) > 0) {
    originalData$cases_total <- popNA(originalData$cases_total,originalData$state)
  }
  if (sum(is.na(originalData$deaths_total)) > 0) {
    originalData$deaths_total <- popNA(originalData$deaths_total,originalData$state)
  }
  if (sum(is.na(originalData$recovered_total)) > 0) {
    originalData$recovered_total <- popNA(originalData$recovered_total,originalData$state)
  }
} else if (libraryToSourceFrom == 'covidregionaldata') { # https://cran.r-project.org/web/packages/covidregionaldata/
  library(covidregionaldata)
  originalData <- get_regional_data(countryName)
  if (countryName == 'Afghanistan') {
    originalData <- originalData %>% rename(state = province)
    # Afghanistan does a funny thing where there are sort of two rows for each Province - e.g. 'Baghlan' and 'Baghlan Province'.  The row without the word 'Province' appears to be more accurate.
    # also artificially excluding Jawzjan  (apparent repeat of Jowzjan, with much less data)
    # also artificially excluding Dykundi  (apparent repeat of Daykundi, with much less data)
    # also artificially excluding Hirat    (apparent repeat of Herat, with much less data)
    # also artificially excluding Nimroz   (apparent repeat of Nimruz, with much less data)    
    # also artificially excluding Paktya   (apparent repeat of Paktia, with much less data)
    # also artificially excluding Panjsher (apparent repeat of Panjshir, with much less data)
    originalData <- subset(originalData, originalData$state %in% c('Badakhshan','Badghis','Baghlan','Balkh','Bamyan','Daykundi','Farah','Faryab','Ghazni','Ghor','Helmand','Herat','Jowzjan','Kabul','Kandahar','Kapisa','Khost','Kunar','Kunduz','Laghman','Logar','Nangarhar','Nimruz','Nuristan','Paktia','Paktika','Panjshir','Parwan','Samangan','Sar-e-Pul','Takhar','Urozgan','Wardak','Zabul'))
    originalData$iso_3166_2 <- NULL
  }
  states <- unique(originalData$state)
  originalData[is.na(originalData)] <- 0 # replace NAs with 0
}

# some missing values can put the dates out of order.
dates <- sort(unique(originalData$date))
seq <- c(1:length(dates))



if (!updateCSV) {

  print('going to update!')

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
  currentConfirmed <- numeric(length(states))
  dataConfirmed <- originalData %>% select(state, date, cases_total)
  outputConfirmed <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    src_set <- subset(dataConfirmed, dataConfirmed$date == dates[value])
    if (length(src_set$cases_total) == length(currentConfirmed)) {
      currentConfirmed <- src_set$cases_total
    } else {
      currentConfirmed[(outputConfirmed$state %in% src_set$state)] <- src_set$cases_total
    }
    outputConfirmed[[format(dates[value], '%-m/%d/%y')]] <- currentConfirmed
  }
  write.csv(outputConfirmed, paste0('csvData/',countryName, '_confirmed.csv'), quote=FALSE, row.names=FALSE)

  #deaths
  currentDeaths <- numeric(length(states))
  dataDeaths <- originalData %>% select(state, date, deaths_total)
  outputDeaths <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    src_set <- subset(dataDeaths, dataDeaths$date == dates[value])
    if (length(src_set$deaths_total) == length(currentDeaths)) {
      currentDeaths <- src_set$deaths_total
    } else {
      currentDeaths[(outputDeaths$state %in% src_set$state)] <- src_set$deaths_total
    }
    outputDeaths[[format(dates[value], '%-m/%d/%y')]] <- currentDeaths
  }
  write.csv(outputDeaths, paste0('csvData/',countryName, '_deaths.csv'), quote=FALSE, row.names=FALSE)

  #recovered
  currentRecovered <- numeric(length(states))
  dataRecovered <- originalData %>% select(state, date, recovered_total)
  outputRecovered <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
  for (value in seq) {
    src_set <- subset(dataRecovered, dataRecovered$date == dates[value])
    if (length(src_set$recovered_total) == length(currentRecovered)) {
      currentRecovered <- src_set$recovered_total
    } else {
      currentRecovered[(outputRecovered$state %in% src_set$state)] <- src_set$recovered_total
    }
    outputRecovered[[format(dates[value], '%-m/%d/%y')]] <- currentRecovered
  }
  write.csv(outputRecovered, paste0('csvData/',countryName, '_recovered.csv'), quote=FALSE, row.names=FALSE)

}

}

# Fill any NA values in src data with copy of value from previous day
# NAs at start of timeline are filled with 0.
popNA <- function(fullData, states) {
  state_list <- unique(states)
  for (s in state_list) {
    timeSeries <- fullData[states == s]
    if (is.na(timeSeries[1])) {
      timeSeries[1] <- 0
    }
    for (i in 2:length(timeSeries)) {
      if (is.na(timeSeries[i])) {
        timeSeries[i] <- timeSeries[i-1]
      }
    }
    fullData[states == s] <- timeSeries
  }
  fullData
}
