library(COVID19)
library(tidyverse)

createCSV <- function(countryName) {

originalData <- covid19(c(countryName), level = 2)

states <- unique(originalData$administrative_area_level_2)
dates <- unique(originalData$date)
seq <- c(1:length(dates))

dataConfirmed <- originalData %>% select(administrative_area_level_2, date, confirmed)
outputConfirmed <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
for (value in seq) {
  outputConfirmed[[format(dates[value], '%-m/%d/%y')]] <- subset(dataConfirmed, dataConfirmed$date == dates[value])$confirmed
}
write.csv(outputConfirmed, paste0('csvData/',countryName, '_confirmed.csv'), quote=FALSE, row.names=FALSE)

dataDeaths <- originalData %>% select(administrative_area_level_2, date, deaths)
outputDeaths <- data.frame('Province/State' = states, 'Country/Region' = replicate(length(states),countryName))
for (value in seq) {
  outputDeaths[[format(dates[value], '%-m/%d/%y')]] <- subset(dataDeaths, dataDeaths$date == dates[value])$deaths
}
write.csv(outputDeaths, paste0('csvData/',countryName, '_deaths.csv'), quote=FALSE, row.names=FALSE)

}
