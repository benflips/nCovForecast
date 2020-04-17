library(readxl)

d <- read_excel(path = "dat/WHORegions/WHORegions.xlsx")

misMatchWHO <- d$Country[! d$Country %in% timeSeriesInfections$Region]
misMatchJHU <- timeSeriesInfections$Region[! timeSeriesInfections$Region %in% d$Country]

# did a bunch of hand-editing to get most names aligned

