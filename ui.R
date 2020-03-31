## ---------------------------
##
## Script name: ui.R
##
## Purpose of script:  Specifies user interface for coronaRisk app
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
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
library(shiny)

## ---------------------------

## load up our functions into memory
## source files
source("functions.R")
source("getDataNew.R")
#load("dat/menuData.RData")

## ---------------------------
## ---------------------------
options(scipen=9)

htmlTemplate('base.html', as_of_date=format(dates[length(dates)], "%d %B %Y"))

