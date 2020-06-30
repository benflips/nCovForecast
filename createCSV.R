library(COVID19)
library(tidyverse)

createCSV <- function(countryName) {

originalData <- covid19(c(countryName), level = 2)
dataConfirmed <- originalData %>% select(administrative_area_level_2, date, confirmed)

states <- unique(dataConfirmed$administrative_area_level_2)
dates <- unique(dataConfirmed$date)
seq <- c(1:length(dates))

outputConfirmed <- data.frame('Province.State' = states)
for (value in seq) {
  outputConfirmed[[format(dates[value], '%-m/%d/%y')]] <- subset(dataConfirmed, dataConfirmed$date == dates[value])$confirmed
}
write.csv(outputConfirmed, paste0(countryName, '_confirmed.csv'), quote=FALSE)

}
