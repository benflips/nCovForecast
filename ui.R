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

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coronavirus 10-day forecast"),
  navbarPage(p("As of", format(dates[length(dates)], "%d %b")),
##### 10-day forecast #####             
      tabPanel("10-day forecast",
             # Sidebar 
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Location"),
                 selectInput(inputId = "countryFinder",
                             label = "Select Country/Region:",
                             choices = ddReg, 
                             selected = ddNames[1]),
                 h5("Raw case numbers:"),
                 tableOutput(outputId = "rawStats"),
                 h5("Active cases:"),
                 tableOutput(outputId = "tablePredConf"),
                 h5("Doubling time (days):"),
                 textOutput(outputId = "doubTime"),
                 titlePanel("Detection"),
                 h5("Estimated proportion of cases detected:"),
                 textOutput(outputId = "detRate"),
                 h5("Possible true number of cases now given imperfect detection:"),
                 textOutput(outputId = "tablePredTrue"),
                 hr(),
                 p("Take this last number with a grain of salt; it is rough.  But low detection indicates that there are many more deaths in the country than there should be given reported case numbers (so there must be more cases than are reported)."),
                 p("Active cases are total number of infections minus deaths and recoveries."),
                 p("For more information, see", 
                      a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("rawPlot"),
                 plotOutput("logPlot")
               )
             )
      ),
##### Growth Rate ##### 
      tabPanel("Growth rate and curve-flattening",
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
                   h5("Growth rate"),
                   p("This is the growth rate of the number of active cases for the last 10 days.  It can be though of as the interest rate, compounded daily."),
                   p("Positive is bad, negative is good. Progress in control would be indicated by steady decline in growth rate over time, and holding in negative territory."),
                   p("Note, days with low or zero growth followed by large spikes are reporting issues: countries miss a day (or several) of reporting and then aggregate cases into the following day."),
                   plotOutput("growthRate"),
                   hr(),
                   h5("Curve flattening index"),
                   p("This is a measure of how well a country is flattening the pandemic curve at any point in time.  Positive values mean growth rates are declining at that point in time."),
                   plotOutput("cfi"),
                   p("For more details see", 
                        a("here.", href = "https://blphillipsresearch.wordpress.com/2020/03/12/coronavirus-forecast/", target="_blank"))
                 )
               )
      )

  )
))
