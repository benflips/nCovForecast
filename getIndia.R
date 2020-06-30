source('getDataGeneral.R')

getDataGeneral('India',
               'https://raw.githubusercontent.com/vipinbhatnagar/covid19/master/confirmed.csv',
               'https://raw.githubusercontent.com/vipinbhatnagar/covid19/master/deaths.csv',
               '',
               TRUE)
