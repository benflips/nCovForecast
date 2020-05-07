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


popDat <- read.csv(file = "dat/Data_Extract_From_World_Development_Indicators.csv", stringsAsFactors = FALSE)
popDat <- popDat %>% filter(grepl(pattern = "\\d", x = popDat$Series.Code)) %>% 
    select(Country.Name, Series.Code, X2018) %>% 
    group_by(Country.Name) %>% 
    spread(key = Series.Code, value = X2018, ) %>%
    mutate(totalN = SP.POP.0014.TO + SP.POP.1564.TO + SP.POP.65UP.TO)
colnames(popDat)[grepl(pattern = "SP", x = colnames(popDat))]<-c("x0014", "x1564", "x65UP")

save(popDat, file = "dat/popData.RData")
