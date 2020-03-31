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
```
6. `library(shiny)`
7. `runApp('.')`  This should automatically open your browser and display the app.

After you have installed and run this for the first time, only steps 3, 4, 6, and 7 will be required.
