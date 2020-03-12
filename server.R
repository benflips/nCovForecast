## ---------------------------
##
## Script name: 
##
## Purpose of script:
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

## source files
source("getData.R")
## ---------------------------



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$rawPlot <- renderPlot({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    if (input$detection) yA<-yA/detRate(yI, yD)
    lDat <- projSimple(yA, dates)
    yMax <- max(c(lDat$y[,"fit"], yA))
    yTxt <- if (input$detection) "Estimated active cases" else "Confirmed active cases"
    plot(yA~dates, 
         xlim = c(min(dates), max(lDat$x)),
         ylim = c(0, yMax),
         pch = 19, 
         bty = "l", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$countryFinder)
    lines(lDat$y[, "fit"]~lDat$x)
    lines(lDat$y[, "lwr"]~lDat$x, lty = 2)
    lines(lDat$y[, "upr"]~lDat$x, lty = 2)
  })
  
  output$logPlot <- renderPlot({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    if (input$detection) yA<-yA/detRate(yI, yD)
    lDat <- projSimple(yA, dates)
    yMax <- max(c(lDat$y[,"fit"], yA))
    yTxt <- if (input$detection) "log(Estimated active cases)" else "log(Confirmed active cases)"
    plot(log(yA)~dates, 
         xlim = c(min(dates), max(lDat$x)),
         ylim = c(0, log(yMax)),
         pch = 19, 
         bty = "l", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$countryFinder)
    lines(log(lDat$y[, "fit"])~lDat$x)
    lines(log(lDat$y[, "lwr"])~lDat$x, lty = 2)
    lines(log(lDat$y[, "upr"])~lDat$x, lty = 2)
  })
  
  output$detRate <- renderText({
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    detRate(yI, yD)
  })
})
