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

## ---------------------------

# Variables for drop-down menus
source("defineMenus.R")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coronavirus 10-day forecast"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      titlePanel("Location selector"),
       selectInput(inputId = "countryFinder",
                          label = "Select Country/Region:",
                          choices = ddReg, 
                          selected = ddNames[1]),
      titlePanel("Detection"),
      selectInput(inputId = "detection",
                  label = "Account for imperfect detection:",
                  choices = c("Yes" = TRUE, "No" = FALSE),
                  selected = "Yes"),
      h5("Proportion of cases detected"),
      textOutput(outputId = "detRate")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("rawPlot"),
       plotOutput("logPlot")
    )
  )
))
