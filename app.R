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
library(shiny.i18n)
library(plotly)

## ---------------------------

## source files
source("functions.R") # makes functions available to the instance.
source("getDataLocal.R") #makes data available to the instance.

## ---------------------------
options(scipen=9)


# Define server logic 
server <- function(input, output, session) {

  i18n <- Translator$new(translation_json_path = "translations/translations.json")
  selected_language <- 'tr'
  observe({i18n$set_translation_language(selected_language)})

  please_select_a_country <- i18n$t('Please select a country or region...')
  clrDark   <- "#273D6E"
  clrLight  <- "#B2C3D5"
  clrOrange <- "#FF7F0E"  

  list2env(dataList[["Global"]], envir = environment()) # make global data available to session

  output$siteName        <- renderText({i18n$t('Coronavirus 10-day forecast')})
  output$byline          <- renderText({i18n$t('Provides estimates of COVID-19 growth rate, detection, and near-future case load in each country, updated daily, based on global data collated by John Hopkins University')})
  output$specificSites   <- renderText({i18n$t('We have specific sites for certain countries.  Please visit')})
  output$TenDayForecasts <- renderText({i18n$t('10-day forecasts')})
  output$growthRates     <- renderText({i18n$t('Growth rates')})
  output$about           <- renderText({i18n$t('About')})
  output$location        <- renderText({i18n$t('Location')})

# #### Observer function -- set country names from url ####
   observe({
     cname <- strsplit(session$clientData$url_hostname, '\\.')[[1]][1]
     if (cname == "au") {
       output$country_name_in_header <- renderText({'Australia'})
       updateSelectizeInput(session, "global_or_country",  selected = "Australia")
     } else if (cname == "us") {
       output$country_name_in_header <- renderText({'United States of America'})
       updateSelectizeInput(session, "global_or_country",  selected = "US")
     } else if (cname == "ca") {
       output$country_name_in_header <- renderText({'Canada'})
       updateSelectizeInput(session, "global_or_country",  selected = "Canada")
     } else if (cname == "cn") {
       output$country_name_in_header <- renderText({'China'})
       updateSelectizeInput(session, "global_or_country",  selected = "China")
     } else if (cname == "in") {
       output$country_name_in_header <- renderText({'India'})
       updateSelectizeInput(session, "global_or_country",  selected = "India")
     } 
   })
  
#### Observer function -- Global or Country level ####
  # if we observe that global_or_country is changing, then update the choices in countryFinder
  observe({
    # change data inputs
    list2env(dataList[[input$global_or_country]], envir = parent.env(environment()))
    output$asOfDate <- renderText(
      if (selected_language == 'en') {
        paste("As of",format(dates[length(dates)], "%d %B %Y"))
      } else {
        paste(format(dates[length(dates)], "%d/%m/%Y"),"itibariyle")
      }
    )
    

    if (input$global_or_country == 'Global') {
      updateSelectizeInput(session, "countryFinder",  choices = ddReg)
      updateSelectizeInput(session, "countryGrowthRate", selected = c("US", "Italy", "Australia", "China"), choices = ddReg)
    } else {
      if (input$global_or_country == 'Australia') {
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("New South Wales","Victoria"), choices = ddReg )
      } else if (input$global_or_country == 'Canada') {
        ddReg = ddReg[! ddReg %in% c('Diamond Princess','Recovered')] # for some reason, these states do not work.  TOFIX.
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("Ontario","Quebec"), choices = ddReg )
      } else if (input$global_or_country == 'China') {
        ddReg = ddReg[! ddReg %in% c('Anhui', 'Guangxi', 'Guizhou', 'Hainan', 'Ningxia', 'Qinghai', 'Tibet', 'Xinjiang')] # for some reason, these states do not work.  TOFIX.
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("Hubei","Henan","Heilongjiang"), choices = ddReg)
      } else if (input$global_or_country == 'US') {
        ddReg = ddReg[! ddReg %in% c('American Samoa')] # for some reason, these states do not work.  TOFIX.
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("Michigan","New Jersey","New York"), choices = ddReg)
      } else if (input$global_or_country == 'India') {
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", selected = c("Maharashtra","Gujarat","Delhi"), choices = ddReg)
      } else {
        updateSelectizeInput(session, "countryFinder",     choices = ddReg)
        updateSelectizeInput(session, "countryGrowthRate", choices = ddReg)
      }
    }
  })


#### Reactive expressions for forecast page ####
  yfCast <-reactive({ # subset country for forecast page
    yI <- tsSub(timeSeriesInfections,timeSeriesInfections$Region %in% input$countryFinder)  
    yD <- tsSub(timeSeriesDeaths,timeSeriesDeaths$Region %in% input$countryFinder) 
    yR <- tsSub(timeSeriesRecoveries,timeSeriesRecoveries$Region %in% input$countryFinder)  
    yA <- tsSub(timeSeriesActive,timeSeriesActive$Region %in% input$countryFinder)
    list(yI = yI, yD = yD, yR = yR, yA = yA)
  })
  
  projfCast <- reactive({ # projection for forecast
    projSimple(yfCast()$yA, dates, inWindow = input$fitWinSlider, timeVaryingGrowth = input$modelType)
  })
  
  # adjust slide input given model type
  observe({
    if (input$modelType){
      updateSliderInput(session, "fitWinSlider", value = 18, min = 10, max = 30)
    } else {
      updateSliderInput(session, "fitWinSlider", value = 7, min = 3, max = 10)
    }
  })
  
  plotRange <- reactive({ # get date range to plot
    yA <- yfCast()$yA
    dFrame <- data.frame(dates = as.Date(names(yA), format = "%m.%d.%y"), yA)
    dFrame_without_na <- na.omit(dFrame$yA)
    if (max(dFrame_without_na)>200) {minDate <- min(dFrame$dates[dFrame_without_na>20]); maxDate <- max(dFrame$dates)+10} else {
      minDate <- min(dFrame$dates); maxDate <- max(dFrame$dates)+10
    }
    list(minDate, maxDate)
  })
  
  ##### Raw stats #####  
  output$rawStats <- renderTable({
    yA <- yfCast()$yA
    yD <- yfCast()$yD
    yI <- yfCast()$yI
    nn <-length(yI)
    if (is.na(yA[nn])) nn <- nn-1
    out <- as.integer(c(yI[nn], yD[nn]))
    dim(out) <-c(1,2)
    colnames(out) <- c(i18n$t("Total infections"), i18n$t("Deaths"))
    format(out, big.mark = ",")
  }, rownames = FALSE)
  
##### Raw plot #####
  output$rawPlot <- renderPlotly({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      yA <- data.frame(dates = as.Date(names(yA), format = "%m.%d.%y"), yA)
      lDat <- projfCast()$lDat
      date_at_peak <- projfCast()$date_at_peak
      value_at_peak <- projfCast()$value_at_peak
      pDat <- merge(yA, lDat, all = TRUE)
      yMax <- max(c(lDat$fit, yA$yA), na.rm = TRUE)*1.05
      #yTxt <- "Confirmed active cases"
      fig <- plot_ly(pDat, type = "scatter", mode = "none") %>%
                add_trace(y = ~fit,
                          x = ~dates, 
                          mode = "lines", 
                          line = list(color = clrDark), 
                          name = i18n$t("Best fit"), 
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$fit, 0), big.mark = ","))) %>%
                add_trace(y = ~lwr,
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark, dash = "dash"), 
                          name = "CI lower bound",
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$lwr, 0), big.mark = ","))) %>%
                add_trace(y = ~upr, 
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark, dash = "dash"), 
                          name = "CI upper bound",
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$upr, 0), big.mark = ","))) %>%
                add_trace(y = ~yA, 
                          x = ~dates,
                          mode = "markers", 
                          marker = list(color = clrLight), 
                          name = i18n$t("Active cases"),
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$yA, 0), big.mark = ","))) %>%
                layout(showlegend = FALSE, 
                       yaxis = list(range = list(0, yMax),
                                    title = list(text = i18n$t("Confirmed active cases")),
                                    fixedrange = TRUE),
                       xaxis = list(range = plotRange(),
                                    title = list(text = ""),
                                    fixedrange = TRUE),
                       title = list(text = input$countryFinder)
                ) %>%
                config(displayModeBar = FALSE)
      if (!is.null(value_at_peak)){
          fig %>% add_trace(y = c(0,value_at_peak), 
                  x = c(date_at_peak,date_at_peak),
                  mode = "lines", 
                  line = list(color = clrLight),
                  name = i18n$t("Estimated peak"),
                  hoverinfo = "text+name",
                  text = format(date_at_peak, "%b, %d"))
      } else {fig}
    }
  })
  
##### Log plot #####
  output$logPlot <- renderPlotly({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      yA <- data.frame(dates = as.Date(names(yA), format = "%m.%d.%y"), yA)
      lDat <- projfCast()$lDat
      value_at_peak <- projfCast()$value_at_peak
      date_at_peak <- projfCast()$date_at_peak
      pDat <- merge(yA, lDat, all = TRUE)
      yMax <- max(c(lDat$fit, yA$yA), na.rm = TRUE)*1.05
      fig <- plot_ly(pDat, type = "scatter", mode = "none") %>%
                add_trace(y = ~fit,
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark), 
                          name = i18n$t("Best fit"),
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$fit, 0), big.mark = ","))) %>%
                add_trace(y = ~lwr, 
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark, dash = "dash"), 
                          name = "CI lower bound",
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$lwr, 0), big.mark = ","))) %>%
                add_trace(y = ~upr, 
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark, dash = "dash"), 
                          name = "CI upper bound",
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$upr, 0), big.mark = ","))) %>%
                add_trace(y = ~yA, 
                          x = ~dates,
                          mode = "markers", 
                          marker = list(color = clrLight), 
                          name = i18n$t("Active cases"),
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, "%b %d"), format(round(pDat$yA, 0), big.mark = ","))) %>%
                layout(showlegend = FALSE, 
                       yaxis = list(type = "log",
                                    range = list(log10(0.1), log10(yMax)),
                                    title = list(text = "Confirmed active cases (log scale)"),
                                    fixedrange = TRUE),
                       xaxis = list(range = plotRange(),
                                    title = list(text = ""),
                                    fixedrange = TRUE)
                ) %>%
                config(displayModeBar = FALSE)
      if (!is.null(value_at_peak)){
        fig %>% add_trace(y = c(0,value_at_peak), 
                  x = c(date_at_peak,date_at_peak),
                  mode = "lines", 
                  line = list(color = clrLight),
                  name = i18n$t("Estimated peak"),
                  hoverinfo = "text+name",
                  text = format(date_at_peak, "%b, %d"))
      } else {fig}
    }
  })

##### New cases ##### 
  output$newCases <- renderPlotly({
    if (input$countryFinder != '') {
      yI <- yfCast()$yI # Infected
      yD <- yfCast()$yD # Deaths
      
      # Calculate new cases
      newCases <- diff(yI) 
      # Format the date
      newCases <- data.frame(dates = as.Date(names(newCases), format = "%m.%d.%y"), newCases) 
      
      # Calculate daily deaths
      dailyDeaths <- diff(yD) 
      
      # Plot new cases
      fig1 <- plot_ly(newCases) %>%
        add_bars(y = ~newCases,
                 x = ~dates, 
                 name = i18n$t("New cases"), 
                 marker = list(color = clrOrange), 
                 text = paste(format(newCases$dates, "%b %d"),format(newCases$newCases, big.mark = ",")),
                 hoverinfo = "text+name"
        ) %>%
        layout(yaxis = list(title = list(text = i18n$t("New cases")))
        )  
      
      # Plot daily deaths
      fig2 <- plot_ly(newCases) %>%
        add_bars(y = ~dailyDeaths,
                 x = ~dates,
                 name = i18n$t("Daily deaths"),
                 marker = list(color = clrDark),
                 text = paste(format(newCases$dates, "%b %d"),format(dailyDeaths, big.mark = ",")),
                 hoverinfo = "text+name"
        ) %>%
        layout(xaxis = list(range = plotRange(),
                            title = list(text = i18n$t("Date"))),
               yaxis = list(title = list(text = i18n$t("Deaths")), side = 'left')
        )
      
      # Composite
      fig <- subplot(fig1, fig2, heights = c(0.8,0.2), nrows=2, shareX = TRUE, titleY = TRUE
      ) %>%
        config(displayModeBar = FALSE) %>%
        layout(legend = list(x=0))
    }
  })
  
##### Detection Plot #####   
  output$detPlot <- renderPlotly({
    if (input$countryFinder != '') {
      # get data subsets
      datI <- yfCast()$yI
      datD <- yfCast()$yD
      # generate detection vector
      detVec <- detRate(infd = datI, deaths = datD, pointEst = FALSE, caseFatalityRatio = input$fatalityRatioSlider)*100
      # smooth with moving average
      detVec <- stats::filter(detVec, rep(1 / 3, 3), sides = 1) #3-day moving average
      # organise into dataframe for plotting
      pDet <- data.frame(dates = as.Date(names(datI), format = "%m.%d.%y"), detVec)
      xRange <- as.list(range(na.omit(pDet)$dates))
      # make the plot
      fig <- plot_ly(pDet, type = "scatter", mode = "none", showlegend = FALSE)
      fig <- fig %>% add_trace(y = ~detVec,
                               x = ~dates,
                               mode = "lines+markers", 
                               name = i18n$t("Detection"),
                               hoverinfo = "text+name", 
                               text = paste(format(pDet$dates, "%b %d"), round(pDet$detVec, 1), "%"))
      fig <- fig %>% layout(xaxis = list(title = list(text = "Date"),
                                         range = xRange),
                            yaxis = list(title = list(text = i18n$t("Cases successfully detected %")))
      ) %>%
        config(displayModeBar = FALSE)
    }
  })
  

  log100cases <- reactive({
    subset(timeSeriesInfections, timeSeriesInfections$Region %in% input$countryGrowthRate)
  })

##### Log100 cases Plot ##### 
  output$log100casesPlot <- renderPlotly({
      yI <- yfCast()$yI
      yI <- subset(yI, yI >= 100)
      yI <- data.frame(yI)
      fig <- plot_ly(yI, type = "scatter", mode = "none")
      fig <- fig %>% layout(showlegend = TRUE, 
                       height = 600,
                       yaxis = list(type = "log",
                                    title = list(text = i18n$t("Confirmed cases (log scale)")),
                                    fixedrange = TRUE),
                       xaxis = list(range = plotRange(),
                                    title = list(text = i18n$t("Number of days since 100 cases")),
                                    fixedrange = TRUE),
                       legend = list(orientation="h", xanchor="center",x=0.5,y=-0.2)
                )
      fig <- fig %>% config(displayModeBar = FALSE)
      doubling_lines <- c(2,3,5)
      ymax <- max(log100cases()[,-1],na.rm=TRUE)
      ymax <- 2^(log2(ymax)*1.05) # just a little higher
      for (doubling_line in doubling_lines) {
        linename <- paste(i18n$t('Doubling every'),doubling_line,i18n$t('days'))
        fig <- fig %>% add_trace(x    = c(0,   log2(ymax/100)*doubling_line),
                                 y    = c(100, ymax),
                                 mode = "lines",
                                 line = list(color = clrLight, dash = "dot"),
                                 hoverinfo = "text",
                                 text = linename,
                                 name = linename)
      }
      for (country in input$countryGrowthRate) {
        myY <- subset(log100cases(), log100cases()$Region == country)
        # remove column with name Region
        myY <- myY[,-1]
        # remove dates as these are unnecessary
        myY <- as.vector(t(myY))
        # only get values bigger than 100
        myY <- subset(myY, myY >= 100)

        fig <- fig %>% add_trace(y    = myY,
                                 mode = "lines",
                                 name = country)
      }
      fig
})


##### Forecast metrics ##### 
  output$forecastMetrics <- renderText({
    if (input$countryFinder == '') {
      please_select_a_country
    } else {
    if (input$modelType) {
      if (is.null(projfCast()$value_at_peak)) {
        i18n$t("Active cases estimated to peak beyond the forecast horizon")
      } else if (is.null(projfCast()$date_at_peak)){
        i18n$t("Active cases peaked in the past")
      } else {
        paste(i18n$t("Active cases estimated to peak at"), format(as.integer(projfCast()$value_at_peak), big.mark=","),i18n$t("cases on"), format(projfCast()$date_at_peak, "%d %B"))
      }
    } else {
        doubTime <- round(projfCast()$doubling_time, 1)
        if (doubTime > 0) {
          dTime <- paste(i18n$t("Doubling time is"), doubTime, i18n$t('days'))
        } else {
          dTime <- paste(i18n$t("Halving time is"), -doubTime, i18n$t('days'))
        }
    }
    }
  })


##### Detection rate #####    
  output$detRate <- renderText({
    if (input$countryFinder == '') {
      please_select_a_country
    } else {
      yI <- yfCast()$yI
      yD <- yfCast()$yD
      dR<-round(detRate(yI, yD, caseFatalityRatio = input$fatalityRatioSlider), 4)*100
      if (is.na(dR)) i18n$t("Insufficient data for estimation") else paste(dR,'%')
    }
  })
  
##### Prediction table confirmed #####    
  output$tablePredConf <- renderTable({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      lDat <- projfCast()$lDat
      nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$lwr,1), tail(lDat$upr,1))), big.mark = ",")
      nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
      dim(nowThen) <- c(1, 2)
      colnames(nowThen)<-c(i18n$t("Now"), i18n$t("In 10 days (min-max)"))
      nowThen
    }
  }, rownames = FALSE)
  
##### Prediction table true #####    
  output$tablePredTrue <- renderTable({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      yD <- yfCast()$yD
      yI <- yfCast()$yI
      dRate <- detRate(yI, yD, caseFatalityRatio = input$fatalityRatioSlider)
      nowDiag <- tail(yA[!is.na(yA)], 1)
      nowUndet <- nowDiag/dRate - nowDiag
      nowUndiag <- active.cases[active.cases$Region==input$countryFinder, ncol(active.cases)] - nowDiag
      if (nowUndiag<0) nowUndiag <- 0
      nowTotal <- nowDiag+nowUndiag+nowUndet
      nowTable <- format(round(c(nowDiag, nowUndiag, nowUndet, nowTotal), 0), big.mark = ",")
      dim(nowTable) <- c(4, 1)
      rownames(nowTable)<-c(i18n$t("Diagnosed"), i18n$t("Undiagnosed"), i18n$t("Undetected"), i18n$t("Total"))
      nowTable
    }
  }, rownames = TRUE, colnames = FALSE)
  
##### Reactive expressions for growth page #####    
  growthSub <- reactive({
    subset(timeSeriesActive, timeSeriesActive$Region %in% input$countryGrowthRate)
  })


##### Curve-flattening #####    
  output$cfi <- renderPlotly({
    pDat <- growthSub()
    pMat<-as.matrix(log(pDat[,-1]))
    row.names(pMat)<-pDat$Region
    cfiDat<-apply(pMat, MARGIN = 1, FUN = "cfi")
    cfiDat[!is.finite(cfiDat)]<-0
    dateSub<-3:length(dates) # date subset
    for (cc in 1:ncol(cfiDat)){
      cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
      cfiDat[,cc] <- predict(cfiSmooth, newdata = as.numeric(dates[dateSub]))
    }
    yRange <- as.list(range(cfiDat)*1.05)
    cfiDat <- data.frame(dates = dates[dateSub], cfiDat)
    fig <- plot_ly(type = "scatter", mode = "none", name = "")
    for (cc in 2:ncol(cfiDat)){
      fig <- fig %>% add_trace(y = cfiDat[,cc],
                               x = dates[dateSub],
                               mode = "lines",
                               name = colnames(cfiDat)[cc],
                               hoverinfo = "text+name", 
                               text = paste(format(cfiDat$dates, "%b %d"), round(cfiDat[,cc], 2)))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = i18n$t("Date"))),
                          yaxis = list(title = list(text = i18n$t("Curve-flattening index")),
                                       range = yRange)
                    ) %>%
                    config(displayModeBar = FALSE)
    
  })
  
##### Growth rate #####    
  output$growthRate <- renderPlotly({
    pDat <- growthSub()
    gRate <- as.matrix(growthRate(pDat[,-1], inWindow = ncol(pDat)-2)) #daily growth over all time
    gRateMA <- apply(gRate, # get moving average (three day window)
                     MARGIN = 1, 
                     FUN = function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)})
    gRateMA[is.infinite(gRateMA)]<-NA #remove Infs
    # reorganise for plotting
    gRateMA <- data.frame(dates = as.Date(colnames(gRate), format = "%m.%d.%y"), gRateMA)
    if (nrow(gRateMA)>20) gRateMA <-tail(gRateMA, 20)
    colnames(gRateMA)[-1] <- pDat$Region
    # and plot
    fig <- plot_ly(gRateMA, type = "scatter", mode = "none")
    for (cc in 2:ncol(gRateMA)){
      fig <- fig %>% add_trace(y = gRateMA[,cc],
                               x = ~dates,
                               mode = "lines", 
                               name = colnames(gRateMA)[cc],
                               hoverinfo = "text+name", 
                               text = paste(format(gRateMA$dates, "%b %d"), round(gRateMA[,cc], 1), "%"))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = "Date")),
                          yaxis = list(title = list(text = "Growth rate (% per day)"))
                    ) %>%
                   config(displayModeBar = FALSE)
  })

  
} # end of server expression


shinyApp(ui = htmlTemplate('base.html'), server)

