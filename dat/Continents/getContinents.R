## ---------------------------
##
## Script name: getContinents
##
## Purpose of script: Get continent, country names for alignment with JHU data
##
## Author: Ben Phillips
##
## Date Created: 2020-04-18
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

## ---------------------------

## load up our functions into memory

## ---------------------------  

d <- read.csv(file = "https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv",
              stringsAsFactors = FALSE)

misMatchData <- d$Country[! d$Country %in% timeSeriesInfections$Region]
misMatchJHU <- timeSeriesInfections$Region[! timeSeriesInfections$Region %in% d$Country]

d$Country[d$Country=="Burkina"] <- "Burkina Faso"
d$Country[d$Country=="Congo, Democratic Republic of"] <- "Congo (Kinshasa)"
d$Country[d$Country=="Congo"] <- "Congo (Brazzaville)"
d$Country[d$Country=="Burma (Myanmar)"] <- "Burma"
d$Country[d$Country=="Cape Verde"] <- "Cabo Verde"
d$Country[d$Country=="Ivory Coast"] <- "Cote d'Ivoire"
d$Country[d$Country=="CZ"] <- "Czechia"
d$Country[d$Country=="Swaziland"] <- "Eswatini"
d$Country[d$Country=="Macedonia"] <- "North Macedonia"
d$Country[d$Country=="Russian Federation"] <- "Russia"
d$Country[d$Country=="East Timor"] <- "Timor-Leste"

d <- rbind(d, cbind(Continent = "Europe", Country = "Kosovo"))
d <- rbind(d, cbind(Continent = "Europe", Country = "Holy See"))
d <- rbind(d, cbind(Continent = "Asia", Country = "Taiwan*"))
d <- rbind(d, cbind(Continent = "Asia", Country = "West Bank and Gaza"))
d <- rbind(d, cbind(Continent = "Africa", Country = "Western Sahara"))

continentData <- d

save(continentData, file = "dat/Continents/continentData.RData")

