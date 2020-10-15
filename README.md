# nCovForecast
Code for nCov forecasting tool (Shiny app)

Takes data from several sources (Johns Hopkins University, Indian Government), breaks it down by country/region and:

1. gives simple projections of active confirmed cases for next ten days;
2. estimates detection probability over time in each country and reports detection-corrected values
3. shows growth rates in each country over last twenty days (tracking success at suppression)

The app is under continual development.  Contributions and suggestions are welcome.  If you have data that you would like to see represented here, please get in contact. 

## To run in development mode

1. Install R on your machine.  This app needs at least version 3.6.0.  Check your R version with `R --version`
2. Get the application files: `git clone https://github.com/benflips/nCovForecast.git`
3. cd to your new folder `nCovForecast`
4. Run it by typing "R" in a terminal
5. Install some packages:
```
install.packages('shiny')
install.packages('readr')
install.packages('markdown')
install.packages('curl')
install.packages('addreg')
install.packages('plotly')
install.packages('shiny.i18n')
install.packages('quantreg')
install.packages('gam')
install.packages('COVID19')
install.packages('broom')
install.packages('lubridate')
install.packages('rvest')
install.packages('covidregionaldata')
```
6. Generate cached data (see below)
7. `library(shiny)`
8. `runApp('.')`  This should automatically open your browser and display the app.

After you have installed and run this for the first time, only steps 3, 4, and 7-8 will be required.  Data can be updated each day by running step 6.

## To generate cached data (step 6)

If you are on the main server, simply run:

`Rscript getData.R`

Otherwise, go into getData.R, set `server <- FALSE`.  This will then obtain data directly from raw.githubusercontent.com rather than from a saved path (which only applies if you are on the main server).  Then run:

`Rscript getData.R`

Which will take about 10 minutes to run.  This script will generate a directory structure inside /dat (if that structure doesn't already exist) and populate it with data required by the app.

In subsequent days, to update data after this initial run you will need to run both `getData.R` and `getIndia.R` separately.
