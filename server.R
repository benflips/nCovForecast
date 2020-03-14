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
# Variables for drop-down menus
source("defineMenus.R")
## ---------------------------



# Define server logic 
shinyServer(function(input, output) {
  
  output$rawPlot <- renderPlot({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    if (input$detection) yA<-yA/detRate(yI, yD)
    lDat <- projSimple(yA, dates)
    yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
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
    yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
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
  
  output$tablePreds <- renderTable({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    dRate <- detRate(yI, yD)
    lDat <- projSimple(yA, dates)
    nowThen <- c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"fit"],1))
    nowThenTrue <- nowThen/dRate
    outTab<-rbind(nowThen, nowThenTrue)
    colnames(outTab)<-c("Now", "Ten days")
    row.names(outTab)<-c("Confirmed cases", "Possible true number")
    outTab
  }, rownames = TRUE, digits = 0)
  
  
  output$cfi <- renderPlot({
    pDat <- subset(tsACountry, tsACountry$Country %in% input$countryFinderCFI)
    pDat <- pDat[rev(order(pDat[,ncol(pDat)-1])),] # order to match menu order
    pMat<-as.matrix(log(pDat[,-1]))
    row.names(pMat)<-pDat$Country
    cfiDat<-apply(pMat, MARGIN = 1, FUN = "cfi")
    cfiDat[!is.finite(cfiDat)]<-0
    clrs<-hcl.colors(length(input$countryFinderCFI))
    dateSub<-3:length(dates) # date subset
    plot(cfiDat[,1]~dates[dateSub], 
         type = "n", 
         ylim = range(c(-1.2,1.2)*sd(cfiDat)),
         bty = "l",
         xlab = "Date",
         ylab = "Curve-flatenning index")
    abline(a = 0, b = 0, lty = 2)
    for (cc in 1:ncol(cfiDat)){
      cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
      lines(cfiSmooth$fitted~dates[dateSub], col = clrs[cc])
    }
    legend("topleft", 
           legend = input$countryFinderCFI, 
           lty = 1, 
           col = clrs,
           bty = "n")
  })
})
