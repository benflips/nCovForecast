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
source("getData.R")
# Variables for drop-down menus
source("defineMenus.R")
## ---------------------------
## ---------------------------


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
                  selected = FALSE),
      h5("Estimated proportion of cases detected"),
      textOutput(outputId = "detRate"),
      titlePanel("Number of active cases"),
      tableOutput(outputId = "tablePreds"),
      h5(p("For more information, see", 
           a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank")))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("rawPlot"),
       plotOutput("logPlot")
    )
  )
))
