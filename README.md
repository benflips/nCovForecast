# nCovForecast
Code for nCov forecasting tool (Shiny app)

Takes data amalgamated for the John Hopkins nCov tracker, breaks it down by country and:

1. gives simple projections of confirmed cases for next ten days;
2. estimates (roughly) detection probability in each country and reports detection-corrected values
3. shows growth rates in each country over last ten days (tracking success at curve flattening)

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
```
6. Generate cached data (see below)
7. `library(shiny)`
8. `runApp('.')`  This should automatically open your browser and display the app.

After you have installed and run this for the first time, only steps 3, 4, and 7-8 will be required.  Data can be updated each day by running step 6.

## To generate cached data (step 6)

If you are on the main server, simply run:

`Rscript getDataNew.R`

Otherwise, go into getData.R, set `server <- FALSE`.  This will then obtain data directly from raw.githubusercontent.com rather than from a saved path (which only applies if you are on the main server).  This script requires a directory structure inside /dat which you will need to setup.  It requires dat/Global/ , and dat/[country/] where country is one of the countries named in the available_countries variable on line 95 of getData.R.

Once this directory structure is in place, run:

`Rscript getData.R`

Which will take about 10 minutes to run.
