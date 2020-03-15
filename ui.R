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

## ---------------------------
## ---------------------------


# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coronavirus 10-day forecast"),
  navbarPage(p("As of", format(dates[length(dates)], "%d %b")),
      tabPanel("10-day forecast",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Location selector"),
                 selectInput(inputId = "countryFinder",
                             label = "Select Country/Region:",
                             choices = ddReg, 
                             selected = ddNames[1]),
                 titlePanel("Detection"),
                 h5("Estimated proportion of cases detected"),
                 textOutput(outputId = "detRate"),
                 titlePanel("Number of active cases"),
                 tableOutput(outputId = "tablePreds"),
                 h5("Active cases are total number of infections minus deaths and recoveries."),
                 h5(p("For more information, see", 
                      a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank")))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("rawPlot"),
                 plotOutput("logPlot")
               )
             )
      ),
      tabPanel("Growth rate",
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Location selector"),
                   checkboxGroupInput(inputId = "countryGrowthRate",
                                      label = "Select Country/Region:",
                                      choices = ddReg,
                                      selected = ddNames[1:3])
                 ),
                 mainPanel(
                   plotOutput("growthRate"),
                   h5("This is the growth rate of the number of active cases for the last 10 days."),
                   h5("Positive is bad, negative is good. Progress in control would be indicated by steady decline in growth rate over time.")
                 )
               )
      ),
      tabPanel("Curve-flattening index",
               # Sidebar
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Location selector"),
                   checkboxGroupInput(inputId = "countryFinderCFI",
                                      label = "Select Country/Region:",
                                      choices = ddReg, 
                                      selected = ddNames[1:3])
                 ),
                 mainPanel(
                   plotOutput("cfi"),
                   h5("This is a measure of how well a country is flattening the pandemic curve at any point in time.  Positive values are good, and China is an excellent reference series."),
                   h5("The index is sensitive to changes in screening/reporting.  
                      It's only as good as the data."),
                   h5(p("For more details see", 
                        a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank")))
                 )
               )
      )
      
  )
))
