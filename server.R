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
source("functions.R")
source("getDataNew.R")

## ---------------------------
options(scipen=9)


# Define server logic 
shinyServer(function(input, output) {

#### Reactive expressions for forecast page ####
  yAfCast <-reactive({ # subset country for forecast page
    tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
  })
  
  projfCast <- reactive({ # projection for forecast
    yA <- yAfCast()
    projSimple(yA, dates, inWindow = input$fitWinSlider)
  })
  
  ##### Raw stats #####  
  output$rawStats <- renderTable({
    yA <- yAfCast()
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    #yR <- tsSub(tsR,tsR$Country.Region %in% input$countryFinder)
    nn <-length(yI)
    if (is.na(yA[nn])) nn <- nn-1
    out <- as.integer(c(yI[nn], yD[nn]))
    dim(out) <-c(1,2)
    colnames(out) <- c("Total", "Deaths")
    format(out, big.mark = ",")
  }, rownames = FALSE)
  
##### Raw plot #####  
  output$rawPlot <- renderPlot({
    yA <- yAfCast()
    lDat <- projfCast()
    yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
    yTxt <- "Confirmed active cases"
    plot(yA~dates, 
         xlim = c(min(dates), max(lDat$x)),
         ylim = c(0, yMax),
         pch = 19, 
         bty = "u", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$countryFinder)
    axis(side = 4)
    lines(lDat$y[, "fit"]~lDat$x)
    lines(lDat$y[, "lwr"]~lDat$x, lty = 2)
    lines(lDat$y[, "upr"]~lDat$x, lty = 2)
  })
  
##### Log plot #####    
  output$logPlot <- renderPlot({
    yA <- yAfCast()
    lDat <- projfCast()
    yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
    yTxt <- "Confirmed active cases (log scale)"
    plot((yA+0.1)~dates, 
         xlim = c(min(dates), max(lDat$x)),
         ylim = c(1, yMax),
         log = "y",
         pch = 19, 
         bty = "u", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$countryFinder)
    axis(side=4)
    lines(lDat$y[, "fit"]~lDat$x)
    lines(lDat$y[, "lwr"]~lDat$x, lty = 2)
    lines(lDat$y[, "upr"]~lDat$x, lty = 2)
  })
  
##### Detection rate #####    
  output$detRate <- renderText({
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    dR<-round(detRate(yI, yD), 4)
    if (is.na(dR)) "Insufficient data for estimation" else dR
  })
  
##### Prediction table confirmed #####    
  output$tablePredConf <- renderTable({
    yA <- yAfCast()
    lDat <- projfCast()
    nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Now", "In 10 days (min-max)")
    nowThen
  }, rownames = FALSE)
  
##### Prediction table true #####    
  output$tablePredTrue <- renderText({
    yA <- yAfCast()
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    dRate <- detRate(yI, yD)
    lDat <- projfCast()
    now <- tail(yA[!is.na(yA)], 1)
    nowTrue <- format(round(now/dRate, 0), big.mark = ",")
    #nowThenTrue <- c(round(nowThenTrue[1],0), paste(round(nowThenTrue[2],0), "-", round(nowThenTrue[3],0)))
    #dim(nowThenTrue) <- c(1, 2)
    #colnames(nowThenTrue)<-c("Now", "In 10 days (min-max)")
    nowTrue
  })
  
##### Reactive expressions for growth page #####    
  growthSub <- reactive({
    subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
  })
##### Curve-flattenning #####    
  output$cfi <- renderPlot({
    pDat <- growthSub()#subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
    pMat<-as.matrix(log(pDat[,-1]))
    row.names(pMat)<-pDat$Country
    cfiDat<-apply(pMat, MARGIN = 1, FUN = "cfi")
    cfiDat[!is.finite(cfiDat)]<-0
    clrs<-hcl.colors(length(input$countryGrowthRate))
    dateSub<-3:length(dates) # date subset
    plot(cfiDat[,1]~dates[dateSub], 
         type = "n", 
         ylim = range(c(-1.2,1.2)*sd(cfiDat)),
         bty = "l",
         xlab = "Date",
         ylab = "Curve-flatenning index")
    abline(a = 0, b = 0, lty = 2, lwd = 2)
    for (cc in 1:ncol(cfiDat)){
      cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
      lines(cfiSmooth$fitted~dates[dateSub], col = clrs[cc], lwd=3)
    }
    legend("topleft", 
           legend = pDat$Country, 
           lty = 1, 
           col = clrs,
           bty = "n")
  })
##### Growth rate #####    
  output$growthRate <- renderPlot({
    pDat <- growthSub()#subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
    gRate <- as.matrix(growthRate(pDat))
    clrs<-hcl.colors(length(input$countryGrowthRate))
    dates10 <- dates[(length(pDat)-10+1):length(pDat)]
    counts <- table(gRate)
    barplot(gRate,
            main="Growth rate",
            xlab="Date", 
            ylab="Growth rate (% per day)",
            beside=TRUE,
            col = clrs,
            legend = pDat$Country,
            args.legend = list(bty = "n", x = "topright"))
  })
  
##### Doubling time ##### 
  output$doubTime <- renderText({
    pDat <- tsSub(tsACountry, tsACountry$Country %in% input$countryFinder)
    dTime <- round(doubTime(pDat, dates, inWindow = input$fitWinSlider), 1)
  })
  
##### Doubling time plot #####    
  output$doubTimePlot <- renderPlot({
    pDat <- subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
    dTime <- as.matrix(doubTime(pDat, dates, inWindow = input$fitWinSlider))
    dTime[!is.finite(dTime)]<-NA
    clrs<-hcl.colors(length(input$countryGrowthRate))
    dates10 <- dates[(length(pDat)-10+1):length(pDat)]
    counts <- table(dTime)
    barplot(dTime,
            main="Doubling time",
            xlab="Date", 
            ylab="Doubling time (days)",
            beside=TRUE,
            col = clrs,
            legend = input$countryGrowthRate,
            args.legend = list(bty = "n", x = "topleft"))
  })
})
