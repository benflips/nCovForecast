library(COVID19)
library(tidyverse)

russian <- covid19(c('Russia'), level = 2)
russianConfirmed <- russian %>% select(administrative_area_level_2, date, confirmed)

federalSubjects <- unique(russianConfirmed$administrative_area_level_2)

dates <- unique(russianConfirmed$date)

final <- data.frame('Province.State' = federalSubjects)

seq <- c(1:length(dates))

for (value in seq) {
  final[[format(dates[value], '%d/%m/%y')]] <- subset(russianConfirmed, russianConfirmed$date == dates[value])$confirmed
}

print(final)
