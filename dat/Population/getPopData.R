## ---------------------------
##
## Script name: getPopData
##
## Purpose of script: extract data from world development indicators and munges into a useful format
##
## Author: Ben Phillips
##
## Date Created: 2020-03-16
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(tidyverse)
## ---------------------------

## load up our functions into memory

## ---------------------------


popDat <- read.csv(file = "dat/Population/Data_Extract_From_World_Development_Indicators.csv", stringsAsFactors = FALSE)
popDat <- popDat %>% filter(grepl(pattern = "\\d", x = popDat$Series.Code)) %>% 
    select(Country.Name, Series.Code, X2018) %>% 
    group_by(Country.Name) %>% 
    spread(key = Series.Code, value = X2018, ) %>%
    mutate(totalN = SP.POP.0014.TO + SP.POP.1564.TO + SP.POP.65UP.TO)
colnames(popDat)[grepl(pattern = "SP", x = colnames(popDat))]<-c("x0014", "x1564", "x65UP")

# align country names between World Bank and JHU
popDat$Country.Name[popDat$Country.Name == 'United States'] <- 'US'
popDat$Country.Name[popDat$Country.Name == 'Korea_ Rep.']   <- 'Korea, South'
popDat$Country.Name[popDat$Country.Name == 'Bahamas_ The']  <- 'Bahamas'
popDat$Country.Name[popDat$Country.Name == 'Brunei Darussalam'] <- 'Brunei'
popDat$Country.Name[popDat$Country.Name == 'Congo_ Rep.'] <- 'Congo (Brazzaville)'
popDat$Country.Name[popDat$Country.Name == 'Congo_ Dem. Rep.'] <- 'Congo (Kinshasa)'
popDat$Country.Name[popDat$Country.Name == 'Czech Republic'] <- 'Czechia'
popDat$Country.Name[popDat$Country.Name == 'Egypt_ Arab Rep.'] <- 'Egypt'
popDat$Country.Name[popDat$Country.Name == 'Gambia_ The'] <- 'Gambia'
popDat$Country.Name[popDat$Country.Name == 'Iran_ Islamic Rep.'] <- 'Iran'
popDat$Country.Name[popDat$Country.Name == 'Kyrgyz Republic'] <- 'Kyrgyzstan'
popDat$Country.Name[popDat$Country.Name == 'Lao PDR'] <- 'Laos'
popDat$Country.Name[popDat$Country.Name == 'Russian Federation'] <- 'Russia'
popDat$Country.Name[popDat$Country.Name == 'St. Kitts and Nevis'] <- 'Saint Kitts and Nevis'
popDat$Country.Name[popDat$Country.Name == 'St. Lucia'] <- 'Saint Lucia'
popDat$Country.Name[popDat$Country.Name == 'St. Vincent and the Grenadines'] <- 'Saint Vincent and the Grenadines'
popDat$Country.Name[popDat$Country.Name == 'Slovak Republic'] <- 'Slovakia'
popDat$Country.Name[popDat$Country.Name == 'Syrian Arab Republic'] <- 'Syria'
popDat$Country.Name[popDat$Country.Name == 'Yemen_ Rep.'] <- 'Yemen'

# Make continent aggregates
load("dat/Continents/continentData.RData")
# get countries with continent associations
temp <- subset(popDat, popDat$Country.Name %in% continentData$Country)
# append continent to dataframe
temp$Continent <- continentData$Continent[match(temp$Country.Name, continentData$Country)]
# make continent aggregates
temp <- temp %>% group_by(Continent) %>% select(-Country.Name)# %>% summarise_all(sum(), na.rm=TRUE)
temp <- aggregate(temp[,1:4], by = list(Country.Name = temp$Continent), FUN = sum, na.rm = TRUE)
# bind onto popDat
popDat <- bind_rows(temp, popDat)

# write out data file
save(popDat, file = "dat/Population/popData.RData")


### Temporary stuff to find mismatches
# source("getDataLocal.R") #makes data available to the instance.
# list2env(dataList[["Global"]], envir = environment())
# JHUNames <- timeSeriesInfections$Region
# popDatNames <- popDat$Country.Name
# popDatNames[!(popDatNames %in% JHUNames)]
# JHUNames[!(JHUNames %in% popDatNames)]
