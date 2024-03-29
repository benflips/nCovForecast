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
  
  ##### Set Language #####
  i18n <- Translator$new(translation_json_path = "translations/translations.json")

  observe({
    url_search <- parseQueryString(session$clientData$url_search)
    if ( is.null(names(url_search))) {
      i18n$set_translation_language('en') # default to English
    } else if ('lang' %in% names(url_search)) {
      selectedLanguage <- parseQueryString(session$clientData$url_search)$lang
      if (selectedLanguage %in% i18n$languages) {
        i18n$set_translation_language(selectedLanguage)
      } else {
        i18n$set_translation_language('en') # default to English
      }
    } else {
      i18n$set_translation_language('en') # default to English
    }
  })

  ##### Some useful variables #####
  please_select_a_country <- reactive({please_select_a_country <- i18n$t('Please select a country or region...')})
  clrDark   <- "#273D6E"
  clrLight  <- "#B2C3D5"
  clrOrange <- "#FF7F0E"  
  shortDateFormat <- "%d %b:"
  
  ##### Flags #####
  output$flagAustralia <- renderImage({
    list(src = normalizePath(file.path('./img/australia-flag-xs.png')),                height=50, alt = 'Australian site', title = 'Australian site')
  }, deleteFile = FALSE)

  output$flagUS        <- renderImage({
    list(src = normalizePath(file.path('./img/united-states-of-america-flag-xs.png')), height=50, alt = 'US site',         title = 'US site')
  }, deleteFile = FALSE)

  output$flagChina     <- renderImage({
    list(src = normalizePath(file.path('./img/china-flag-xs.png')),                    height=50, alt = 'Chinese site',    title = 'Chinese site')
  }, deleteFile = FALSE)

  output$flagIndia     <- renderImage({
    list(src = normalizePath(file.path('./img/india-flag-xs.png')),                    height=50, alt = 'Indian site',     title = 'Indian site')
  }, deleteFile = FALSE)

  output$flagCanada    <- renderImage({
    list(src = normalizePath(file.path('./img/canada-flag-xs.png')),                   height=50, alt = 'Canadian site',   title = 'Canadian site')
  }, deleteFile = FALSE)

  output$flagGermany   <- renderImage({
    list(src = normalizePath(file.path('./img/germany-flag-xs.png')),                  height=50, alt = 'German site',     title = 'German site')
  }, deleteFile = FALSE)

  output$flagItaly     <- renderImage({
    list(src = normalizePath(file.path('./img/italy-flag-xs.png')),                    height=50, alt = 'Italian site',    title = 'Italian site')
  }, deleteFile = FALSE)

  output$flagColombia   <- renderImage({
    list(src = normalizePath(file.path('./img/colombia-flag-xs.png')),                 height=50, alt = 'Colombian site',  title = 'Colombian site')
  }, deleteFile = FALSE)

  output$flagJapan   <- renderImage({
    list(src = normalizePath(file.path('./img/japan-flag-xs.png')),                    height=50, alt = 'Japanese site',  title = 'Japanese site')
  }, deleteFile = FALSE)

  output$flagPeru   <- renderImage({
    list(src = normalizePath(file.path('./img/peru-flag-xs.png')),                     height=50, alt = 'Peruvian site',  title = 'Peruvian site')
  }, deleteFile = FALSE)

  output$flagSwitzerland   <- renderImage({
    list(src = normalizePath(file.path('./img/switzerland-flag-xs.png')),              height=50, alt = 'Swiss site',  title = 'Swiss site')
  }, deleteFile = FALSE)

  output$flagBelgium   <- renderImage({
    list(src = normalizePath(file.path('./img/belgium-flag-xs.png')),                  height=50, alt = 'Belgian site',  title = 'Belgian site')
  }, deleteFile = FALSE)

  output$flagHaiti   <- renderImage({
    list(src = normalizePath(file.path('./img/haiti-flag-xs.png')),                    height=50, alt = 'Haitian site',  title = 'Haitian site')
  }, deleteFile = FALSE)

  output$flagBrazil   <- renderImage({
    list(src = normalizePath(file.path('./img/brazil-flag-xs.png')),                    height=50, alt = 'Brazilian site',  title = 'Brazilian site')
  }, deleteFile = FALSE)

  list2env(dataList[["Global"]], envir = environment()) # make global data available to session

  ##### Text to be translated #####
  ### the text is here rather than base.html so that it can easily be translated to other languages
  ## header (anything on a dark blue background)
  output$siteName         <- renderText({i18n$t('Coronavirus 10-day forecast')})
  output$byline           <- renderText({i18n$t('Provides estimates of COVID-19 growth rate, detection, and near-future case load in each country, updated daily, based on global data collated by Johns Hopkins University')})
  output$TenDayForecasts  <- renderText({i18n$t('10-day forecasts')})
  output$growthRates      <- renderText({i18n$t('Growth rates')})
  output$about            <- renderText({i18n$t('About')})

  ## skip to (can't reuse these unfortunately)
  output$skipTo1          <- renderText({i18n$t('Skip to:')})
  output$skipTo2          <- renderText({i18n$t('Skip to:')})
  output$skipTo3          <- renderText({i18n$t('Skip to:')})

  ## section 0
  output$selectALanguage  <- renderText({i18n$t('Select a language:')})
  output$selectGlobal     <- renderText({i18n$t('Select global or country:')})
  output$specificSites    <- renderText({i18n$t('We have specific sites for certain countries.  Please visit')})
  output$otherCountries   <- renderText({i18n$t('Are you interested in other countries?  This site is locked to one country, but go to our main site for all countries with more than 20 cases of covid-19.')})

  ## section 1
  output$location          <- renderText({i18n$t('Location')})
  output$selectCountryRegion1 <- renderText({i18n$t('Select Country/Region:')})
  output$rawCaseNumbers    <- renderText({i18n$t('Raw case numbers:')})
  output$activeCases       <- renderText({i18n$t('Active cases:')})
  output$activeCasesP      <- renderText({i18n$t('Active cases are total number of infections minus deaths and recoveries.')})
  output$forecastMetricsH  <- renderText({i18n$t('Forecast metrics:')})
  output$growthLegend      <- renderText({i18n$t('Growth')})
  output$timeVaryingGrowth <- renderText({i18n$t('Time-varying')})
  output$constantGrowth    <- renderText({i18n$t('Constant')})
  output$timeLegend        <- renderText({i18n$t('Time')})
  output$recentTime        <- renderText({i18n$t('Past 4 months')})
  output$allTime           <- renderText({i18n$t('All')})
  output$fitWindow         <- renderText({i18n$t('Fit window:')})
  output$growthRatesP      <- renderText({i18n$t('When growth rates are changing fast, reduce the fit window to average growth over more recent history')})
  output$detection         <- renderText({i18n$t('Detection')})
  output$casesSuccessfullyDetected <- renderText({i18n$t('Cases successfully detected:')})
  output$possibleNumberOf  <- renderText({i18n$t('Possible number of active cases now given imperfect detection:')})
  output$caseFatalityRatio <- renderText({i18n$t('Case fatality ratio:')})
  output$toSeeHowThe       <- renderText({i18n$t('To see how the assumed case fatality ratio affects detection (and so possible true case numbers) adjust the slider.')})
  output$takeTheseLast     <- renderText({i18n$t('Take these last numbers with a grain of salt; they are rough.  Undiagnosed cases are current infections yet to develop symptoms and be diagnosed.  Undetected cases are current infections that will not be diagnosed.  Large numbers of undetected cases indicate that there are many more deaths in the region than there should be given reported case numbers (so there are many undetected cases or a large number of imported cases).')})
  output$theLastPlot       <- renderText({i18n$t('The last plot is the percentage of new cases that are successfully detected, and how this has changed over time.  Values near 100% are good, indicating that most cases are being detected/reported.  Unexpected outbreaks cause temporary reductions in detection.')})
  output$detectionCanOnly  <- renderText({i18n$t('Detection can only be calculated up to 17 days in the past, and estimates are often patchy in countries/regions with few deaths.')})
  output$logScale          <- renderText({i18n$t('Log scale')})
  output$linearScale       <- renderText({i18n$t('Linear scale')})

  ## section 2
  output$locationH2       <- renderText({i18n$t('Location')})
  output$selectCountryRegion2 <- renderText({i18n$t('Select Country/Region:')})
  output$growthTrajectories <- renderText({i18n$t('Growth trajectories')})
  output$growthRates2     <- renderText({i18n$t('Growth rates')})
  output$thisIsTheGrowth  <- renderText({i18n$t('This is the growth trajectory of selected regions: growth in total cases standardised to start when a region reaches 100 cases.')})
  output$thisPlotDoesNot  <- renderText({i18n$t('This plot does not account for detection.  Because it is on the log scale, the slope at any time is indicative of the doubling time in total cases at that point in time (indicative slopes shown).')})
  output$thisIsTheDaily   <- renderText({i18n$t('This is the daily growth in active cases, plotted over the last 20 days.  It can be thought of as the interest rate, compounded daily.')})
  output$positiveIsBad    <- renderText({i18n$t('Positive is bad, negative is good. Progress in control would be indicated by steady decline in growth rate over time, and holding in negative territory.')})
  output$curveFlatteningIndex <- renderText({i18n$t('Curve flattening index')})
  output$thisIsAMeasure   <- renderText({i18n$t('This is a measure of how well a region is flattening the pandemic curve at any point in time.  Positive values mean growth rates are declining at that point in time.')})
  output$noteThisLast     <- renderText({i18n$t('Note, this last plot covers the entire time period of the pandemic, not just the last twenty days.')})
#  output$<- renderText({i18n$t('')})

  ##### Observer function -- set country names from url ####
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
     } else if (cname == "de") {
       output$country_name_in_header <- renderText({'Germany'})
       updateSelectizeInput(session, "global_or_country",  selected = "Germany")
     } else if (cname == "it") {
       output$country_name_in_header <- renderText({'Italy'})
       updateSelectizeInput(session, "global_or_country",  selected = "Italy")
     } else if (cname == "co") {
       output$country_name_in_header <- renderText({'Colombia'})
       updateSelectizeInput(session, "global_or_country",  selected = "Colombia")
     } else if (cname == "cn") {
       output$country_name_in_header <- renderText({'China'})
       updateSelectizeInput(session, "global_or_country",  selected = "China")
     } else if (cname == "in") {
       output$country_name_in_header <- renderText({'India'})
       updateSelectizeInput(session, "global_or_country",  selected = "India")
     } else if (cname == "jp") {
       output$country_name_in_header <- renderText({'Japan'})
       updateSelectizeInput(session, "global_or_country",  selected = "Japan")
     } else if (cname == "pe") {
       output$country_name_in_header <- renderText({'Peru'})
       updateSelectizeInput(session, "global_or_country",  selected = "Peru")
     } else if (cname == "ht") {
       output$country_name_in_header <- renderText({'Haiti'})
       updateSelectizeInput(session, "global_or_country",  selected = "Haiti")
     } else if (cname == "ch") {
       output$country_name_in_header <- renderText({'Switzerland'})
       updateSelectizeInput(session, "global_or_country",  selected = "Switzerland")
     } else if (cname == "be") {
       output$country_name_in_header <- renderText({'Belgium'})
       updateSelectizeInput(session, "global_or_country",  selected = "Belgium")
     } else if (cname == "br") {
       output$country_name_in_header <- renderText({'Brazil'})
       updateSelectizeInput(session, "global_or_country",  selected = "Brazil")
     } 
   })
  
  #### Observer function -- Global or Country level ####
  # if we observe that global_or_country is changing, then update the choices in countryFinder
  observe({
    # change data inputs
    list2env(dataList[[input$global_or_country]], envir = parent.env(environment()))

    output$asOf           <- renderText({paste(i18n$t('As of'),format(dates[length(dates)], "%d %B %Y"))})

    if (input$global_or_country == 'Global') {
      updateSelectizeInput(session, "countryFinder",  choices = ddReg)
      updateSelectizeInput(session, "countryGrowthRate", selected = c("US", "Italy", "Australia", "China"), choices = ddReg)
    } else {

      regionsWithFinalInfectionsCases <- cbind(timeSeriesInfections$Region, timeSeriesInfections[,ncol(timeSeriesInfections)])
      regionsWithFinalInfectionsCases <- regionsWithFinalInfectionsCases[order(as.numeric(as.character(regionsWithFinalInfectionsCases[,2]))),]

      threeMostInfectionsRegions <- head(tail(regionsWithFinalInfectionsCases,n=4),n=3)
      if (is.vector(threeMostInfectionsRegions)) {
        dim(threeMostInfectionsRegions) <- c(1,2)
      }
      threeMostInfectionsRegions <- rev(as.vector(threeMostInfectionsRegions[,1]))
      
      if (input$global_or_country == 'Canada') {
        ddReg = ddReg[! ddReg %in% c('Diamond Princess','Recovered')] # for some reason, these states do not work.  TOFIX.
      } else if (input$global_or_country == 'China') {
        ddReg = ddReg[! ddReg %in% c('Anhui', 'Guangxi', 'Guizhou', 'Hainan', 'Ningxia', 'Qinghai', 'Tibet', 'Xinjiang')] # for some reason, these states do not work.  TOFIX.
      } else if (input$global_or_country == 'US') {
        ddReg = ddReg[! ddReg %in% c('American Samoa')] # for some reason, these states do not work.  TOFIX.
      }

      updateSelectizeInput(session, "countryFinder", choices = ddReg)
      updateSelectizeInput(session, "countryGrowthRate", selected = threeMostInfectionsRegions, choices = ddReg)

    }  
  
  })


#### Reactive expressions for forecast page ####
  yfCast <-reactive({ # subset country for forecast page
    yI <- tsSub(timeSeriesInfections,timeSeriesInfections$Region %in% input$countryFinder)  
    yD <- tsSub(timeSeriesDeaths,timeSeriesDeaths$Region %in% input$countryFinder) 
    yR <- tsSub(timeSeriesRecoveries,timeSeriesRecoveries$Region %in% input$countryFinder)  
    yA <- tsSub(timeSeriesActive,timeSeriesActive$Region %in% input$countryFinder)
    yIplus <- tsSub(cumulative.infections,cumulative.infections$Region %in% input$countryFinder)
    yUndiag <- tsSub(undiagnosed.infections,undiagnosed.infections$Region %in% input$countryFinder)
    list(yI = yI, yD = yD, yR = yR, yA = yA, yIplus = yIplus, yUndiag = yUndiag)
  })

  projfCast <- reactive({ # projection for forecast
    projSimple(yfCast()$yA, dates, inWindow = input$fitWinSlider, timeVaryingGrowth = input$modelType)
  })
  
  # adjust slide input given model type
  observe({
    if (input$modelType){
      updateSliderInput(session, "fitWinSlider", value = 14, min = 6, max = 20)
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
    if (input$timeRange) minDate <- maxDate - 120 # recent time is last 4 months
    list(minDate, maxDate)
  })

  deathsInCountries <- reactive({
    subset(timeSeriesDeaths, timeSeriesDeaths$Region %in% input$countryGrowthRate)
  })

  
  ##### Days since last... Table #####
  output$daysSinceLast <- renderTable({

    countryNames       <- c()
    daysOfZeroNewCases <- c()
    daysOfZeroDeaths   <- c()

    for (country in input$countryGrowthRate) {
      countryNames <- append(countryNames, country)
      yI <- subset(log100cases(),             log100cases()$Region == country)
      yD <- subset(deathsInCountries(), deathsInCountries()$Region == country)

      # remove column with name Region
      yI <- yI[,-1]
      yD <- yD[,-1]

      dailyNewCases <- diff(as.numeric(yI))
      dailyDeaths   <- diff(as.numeric(yD))

      moreThanZeroNewCases <- which(dailyNewCases > 0)
      moreThanZeroDeaths   <- which(dailyDeaths > 0)

      if (length(moreThanZeroDeaths) == 0) {
        daysOfZeroDeaths   <- append(daysOfZeroDeaths,'N/A')
      } else {
        daysOfZeroDeaths   <- append(daysOfZeroDeaths,  length(yD) - moreThanZeroDeaths[length(moreThanZeroDeaths)]     - 1)
      }

      if (length(moreThanZeroNewCases) == 0) {
        daysOfZeroNewCases <- append(daysOfZeroNewCases, 'N/A')
      } else {
        daysOfZeroNewCases <- append(daysOfZeroNewCases,length(yI) - moreThanZeroNewCases[length(moreThanZeroNewCases)] - 1)
      }

    }

    out <- data.frame(countryNames, daysOfZeroNewCases, daysOfZeroDeaths)
    colnames(out) <- c(i18n$t("Country/region"), i18n$t("Days since last new case"), i18n$t("Days since last death"))
    format(out, big.mark = ",")

  }, rownames = FALSE, align = "lcc")
 


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
  output$activeCasesPlot <- renderPlotly({
    if (input$countryFinder != '') {
      yA <- yfCast()$yA
      yA <- data.frame(dates = as.Date(names(yA), format = "%m.%d.%y"), yA)
      yAmax <- max(yA$yA, na.rm=TRUE)
      lDat <- projfCast()$lDat
      date_at_peak <- projfCast()$date_at_peak
      value_at_peak <- projfCast()$value_at_peak
      pDat <- merge(yA, lDat, all = TRUE)
      yMax <- max(c(lDat$fit, yA$yA), na.rm = TRUE)*1.05
      yMax <- min(c(2*yAmax, yMax)) # twice the data ymax, or the fit max, whatever is smaller
      if (input$activeCasesPlotIsLinear) {
        plotType <- 'linear'
        theRange <- list(0, yMax)
      } else {
        plotType <- 'log'
        theRange <- list(log10(1), log10(yMax))
      }
      fig <- plot_ly(pDat, type = "scatter", mode = "none") %>%
                add_trace(y = ~fit,
                          x = ~dates, 
                          mode = "lines", 
                          line = list(color = clrDark), 
                          name = i18n$t("Best fit"), 
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, shortDateFormat), format(round(pDat$fit, 0), big.mark = ","))) %>%
                add_trace(y = ~lwr,
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark, dash = "dash"), 
                          name = "CI lower bound",
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, shortDateFormat), format(round(pDat$lwr, 0), big.mark = ","))) %>%
                add_trace(y = ~upr, 
                          x = ~dates,
                          mode = "lines", 
                          line = list(color = clrDark, dash = "dash"), 
                          name = "CI upper bound",
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, shortDateFormat), format(round(pDat$upr, 0), big.mark = ","))) %>%
                add_trace(y = ~yA, 
                          x = ~dates,
                          mode = "markers", 
                          marker = list(color = clrLight), 
                          name = i18n$t("Active cases"),
                          hoverinfo = "text+name", 
                          text = paste(format(pDat$dates, shortDateFormat), format(round(pDat$yA, 0), big.mark = ","))) %>%
                layout(showlegend = FALSE, 
                       yaxis = list(type  = plotType,
                                    range = theRange,
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
                 text = paste(format(newCases$dates, shortDateFormat),format(newCases$newCases, big.mark = ",")),
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
                 text = paste(format(newCases$dates, shortDateFormat),format(dailyDeaths, big.mark = ",")),
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

##### Undiagnosed plot #####  
  output$undiagPlot <- renderPlotly({
    if (input$countryFinder != '') {
    rawI <- yfCast()$yI # diagnosed infection totals at t
    estI <- yfCast()$yIplus # estimated true infections at t (diagnosed + undiagnosed)
    rawIdates <- as.Date(names(rawI), format = '%m.%d.%y')
    estIdates <- as.Date(names(estI), format = '%m.%d.%y')
    fig <- plot_ly(type = "scatter", mode = "none")
    if (sum(estI)>0){
      fig <- add_trace(fig,
                       y = ~estI,
                       x = estIdates,
                       mode = "lines", 
                       line = list(color = clrDark), 
                       name = paste(i18n$t("Diagnosed"), " + ", i18n$t("Undiagnosed")), 
                       hoverinfo = "text+name",
                       text = paste(format(estIdates, shortDateFormat), format(round(estI), big.mark = ",")))
    }
      fig <- add_trace(fig, 
                y = ~rawI,
                x = rawIdates,
                mode = "lines+markers",
                marker = list(color = clrLight), 
                line = list(color = clrLight), 
                name = i18n$t("Diagnosed"), 
                hoverinfo = "text+name",
                text = paste(format(rawIdates, shortDateFormat), format(rawI, big.mark = ","))) %>%
      layout(xaxis = list(range = plotRange(),
                          title = list(text = i18n$t("Date"))),
             yaxis = list(title = list(text = i18n$t("Total infections")), 
                          side = 'left'),
             legend = list(x = 0, y = 1.05)) %>%
      config(displayModeBar = FALSE)
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
                               text = paste(format(pDet$dates, shortDateFormat), round(pDet$detVec, 1), "%"))
      fig <- fig %>% layout(xaxis = list(title = list(text = i18n$t("Date")),
                                         range = plotRange()),
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
        x   <- 0:(length(myY)-1)
        fig <- fig %>% add_trace(y    = myY,
                                 x    = x,
                                 mode = "lines",
                                 name = country,
                                 hoverinfo = "text+name",
                                 text = paste(format(myY,big.mark=","), "total cases at", x, "days since 100 cases"))
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
      yUndiag <- yfCast()$yUndiag
      yIplus <- yfCast()$yIplus
      dRate <- detRate(yI, yD, caseFatalityRatio = input$fatalityRatioSlider)
      nowDiag <- tail(yA[!is.na(yA)], 1)
      nowUndet <- nowDiag/dRate - nowDiag
      nowUndiag <- yUndiag[length(yUndiag)]
      if (nowUndiag<0) nowUndiag <- 0
      if (sum(yIplus)==0) nowUndiag <- NA
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
                               text = paste(format(cfiDat$dates, shortDateFormat), round(cfiDat[,cc], 2)))
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
                               text = paste(format(gRateMA$dates, shortDateFormat), round(gRateMA[,cc], 1), "%"))
    }
    fig <- fig %>% layout(xaxis = list(title = list(text = i18n$t("Date"))),
                          yaxis = list(title = list(text = i18n$t("Growth rate (% per day)")))
                    ) %>%
                   config(displayModeBar = FALSE)
  })

  
} # end of server expression


shinyApp(ui = htmlTemplate('base.html'), server)

