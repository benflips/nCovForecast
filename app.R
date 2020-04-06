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
library(plotly)
## ---------------------------

## source files
source("functions.R")
source("getDataLocal.R")

## ---------------------------
options(scipen=9)


# Define server logic 
server <- function(input, output) {

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
  output$rawPlot <- renderPlotly({
    yA <- yAfCast()
    yA <- data.frame(dates = as.Date(names(yA), format = "%m/%d/%y"), yA)
    lDat <- projfCast()
    pDat <- merge(yA, lDat, all = TRUE)
    yMax <- max(c(lDat$fit, yA$yA), na.rm = TRUE)
    clrDark<-"#273D6E"
    clrLight<-"#B2C3D5"
    #yTxt <- "Confirmed active cases"
    fig <- plot_ly(pDat, x = ~dates)
      fig <- fig %>% add_trace(y = ~fit, mode = "lines", line = list(color = clrDark))
      fig <- fig %>% add_trace(y = ~lwr, mode = "lines", line = list(color = clrDark, dash = "dash"))
      fig <- fig %>% add_trace(y = ~upr, mode = "lines", line = list(color = clrDark, dash = "dash"))
      fig <- fig %>% add_trace(y = ~yA, mode = "markers", marker = list(color = clrLight))
      fig <- fig %>% layout(showlegend = FALSE, 
                            yaxis = list(range = list(0, yMax),
                                         title = list(text = "Confirmed active cases")),
                            title = list(text = input$countryFinder)
                            )
  })
  
##### Log plot #####
  output$logPlot <- renderPlotly({
    yA <- yAfCast()
    yA <- data.frame(dates = as.Date(names(yA), format = "%m/%d/%y"), yA)
    lDat <- projfCast()
    pDat <- merge(yA, lDat, all = TRUE)
    yMax <- max(c(lDat$fit, yA$yA), na.rm = TRUE)
    clrDark<-"#273D6E"
    clrLight<-"#B2C3D5"
    #yTxt <- "Confirmed active cases"
    fig <- plot_ly(pDat, x = ~dates)
    fig <- fig %>% add_trace(y = ~fit, mode = "lines", line = list(color = clrDark))
    fig <- fig %>% add_trace(y = ~lwr, mode = "lines", line = list(color = clrDark, dash = "dash"))
    fig <- fig %>% add_trace(y = ~upr, mode = "lines", line = list(color = clrDark, dash = "dash"))
    fig <- fig %>% add_trace(y = ~yA, mode = "markers", marker = list(color = clrLight))
    fig <- fig %>% layout(showlegend = FALSE, 
                          yaxis = list(type = "log",
                                       range = list(log10(0.1), log10(yMax)),
                                       title = list(text = "Confirmed active cases (log scale)")),
                          xaxis = list(title = list(text = "Date"))
                    )
  })
##### Log plot #####    
  output$logPlot2 <- renderPlot({
    yA <- yAfCast()
    lDat <- projfCast()
    yMax <- max(c(lDat$fit, yA), na.rm = TRUE)
    yTxt <- "Confirmed active cases (log scale)"
    plot((yA+0.1)~dates, 
         xlim = c(min(dates), max(lDat$dates)),
         ylim = c(1, yMax),
         log = "y",
         pch = 19, 
         bty = "u", 
         xlab = "Date", 
         ylab = yTxt,
         main = input$countryFinder)
    axis(side=4)
    lines(lDat$fit~lDat$dates)
    lines(lDat$lwr~lDat$dates, lty = 2)
    lines(lDat$upr~lDat$dates, lty = 2)
  })
  
##### Detection rate #####    
  output$detRate <- renderText({
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    dR<-round(detRate(yI, yD), 4)*100
    if (is.na(dR)) "Insufficient data for estimation" else paste(dR,'%')
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
  output$tablePredTrue <- renderTable({
    yA <- yAfCast()
    yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
    yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
    dRate <- detRate(yI, yD)
    lDat <- projfCast()
    nowDiag <- tail(yA[!is.na(yA)], 1)
    nowUndet <- nowDiag/dRate - nowDiag
    nowUndiag <- active.cases[active.cases$Country==input$countryFinder, ncol(active.cases)] - nowDiag
    if (nowUndiag<0) nowUndiag <- 0
    nowTotal <- nowDiag+nowUndiag+nowUndet
    nowTable <- format(round(c(nowDiag, nowUndiag, nowUndet, nowTotal), 0), big.mark = ",")
    dim(nowTable) <- c(4, 1)
    rownames(nowTable)<-c("Diagnosed", "Undiagnosed", "Undetected", "Total")
    nowTable
  }, rownames = TRUE, colnames = FALSE)
  
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
            names.arg = format(as.Date(colnames(gRate), format = "%m/%d/%y"), format = "%b %d"),
            args.legend = list(bty = "n", x = "topright"))
  })
  
##### Doubling time ##### 
  output$doubTime <- renderText({
    pDat <- tsSub(tsACountry, tsACountry$Country %in% input$countryFinder)
    dTime <- paste(round(doubTime(pDat, dates, inWindow = input$fitWinSlider), 1), ' days')
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
}

shinyApp(ui = htmlTemplate('base.html'), server)

