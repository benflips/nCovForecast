source('getDataGeneral.R')

getDataGeneral('Italy',
               './csvData/Italy_confirmed.csv',
               './csvData/Italy_deaths.csv',
               './csvData/Italy_recovered.csv',
               TRUE)
