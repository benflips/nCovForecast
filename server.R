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
options(scipen=9)


# Define server logic 
shinyServer(function(input, output) {
  ##### Raw stats #####  
  output$rawStats <- renderTable({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    yR <- tsSub(tsR,tsR$Country.Region %in% input$countryFinder)
    nn <-length(yI)
    if (is.na(yA[nn])) nn <- nn-1
    out <- as.integer(c(yI[nn], yR[nn], yD[nn]))
    dim(out) <-c(1,3)
    colnames(out) <- c("Total", "Recovered", "Deaths")
    format(out, big.mark = ",")
  }, rownames = FALSE)
  
##### Raw plot #####  
  output$rawPlot <- renderPlot({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    lDat <- projSimple(yA, dates)
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
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    lDat <- projSimple(yA, dates)
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
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    lDat <- projSimple(yA, dates)
    nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
    nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
    dim(nowThen) <- c(1, 2)
    colnames(nowThen)<-c("Now", "In 10 days (min-max)")
    nowThen
  }, rownames = FALSE)
  
##### Prediction table true #####    
  output$tablePredTrue <- renderText({
    yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    dRate <- detRate(yI, yD)
    lDat <- projSimple(yA, dates)
    now <- tail(yA[!is.na(yA)], 1)
    nowTrue <- format(round(now/dRate, 0), big.mark = ",")
    #nowThenTrue <- c(round(nowThenTrue[1],0), paste(round(nowThenTrue[2],0), "-", round(nowThenTrue[3],0)))
    #dim(nowThenTrue) <- c(1, 2)
    #colnames(nowThenTrue)<-c("Now", "In 10 days (min-max)")
    nowTrue
  })
  
##### Curve-flattenning #####    
  output$cfi <- renderPlot({
    pDat <- subset(tsACountry, tsACountry$Country %in% input$countryFinderCFI)
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
    abline(a = 0, b = 0, lty = 2, lwd = 2)
    for (cc in 1:ncol(cfiDat)){
      cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
      lines(cfiSmooth$fitted~dates[dateSub], col = clrs[cc], lwd=2)
    }
    legend("topleft", 
           legend = input$countryFinderCFI, 
           lty = 1, 
           col = clrs,
           bty = "n")
  })
##### Growth rate #####    
  output$growthRate <- renderPlot({
    pDat <- subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
    gRate <- as.matrix(growthRate(pDat))
    clrs<-hcl.colors(length(input$countryGrowthRate))
    dates10 <- dates[(length(pDat)-10+1):length(pDat)]
    counts <- table(gRate)
    barplot(gRate,
            main="Growth rate",
            xlab="Date", 
            ylab="Growth rate",
            beside=TRUE,
            col = clrs,
            legend = input$countryGrowthRate,
            args.legend = list(bty = "n", x = "topleft"))
  })
  
##### Doubling time ##### 
  output$doubTime <- renderText({
    pDat <- tsSub(tsACountry, tsACountry$Country %in% input$countryFinder)
    dTime <- doubTime(pDat)
    round(mean(dTime), 1)
  })
})
