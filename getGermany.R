source('getDataGeneral.R')

getDataGeneral('Germany',
               '/srv/shiny-server/covid19_rki_data/confirmed.csv',
               '/srv/shiny-server/covid19_rki_data/deaths.csv',
               '',
               TRUE,
               deconvProcess =1)
