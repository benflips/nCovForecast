source('getDataGeneral.R')

getDataGeneral('Italy',
               './csvData/Colombia_confirmed.csv',
               './csvData/Colombia_deaths.csv',
               './csvData/Colombia_recovered.csv',
               TRUE)
