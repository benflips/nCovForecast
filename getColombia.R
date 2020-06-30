source('getDataGeneral.R')

getDataGeneral('Colombia',
               './csvData/Colombia_confirmed.csv',
               './csvData/Colombia_deaths.csv',
               './csvData/Colombia_recovered.csv',
               TRUE)
