load('dat/dataList.RData')

namesFromDatalist <- names(dataList)

for (name in namesFromDatalist) {
   message(paste(tail(names(dataList[[name]]$timeSeriesInfections),n=1), name))
}

