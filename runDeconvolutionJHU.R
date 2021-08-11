source('getDataGeneral.R')

runDeconvolution('Global',    deconvProcess = 2, runShort = TRUE)
runDeconvolution('China',     deconvProcess = 2, runShort = TRUE)
runDeconvolution('US',        deconvProcess = 2, runShort = TRUE)
runDeconvolution('Canada',    deconvProcess = 2, runShort = TRUE)
runDeconvolution('Australia', deconvProcess = 2, runShort = TRUE)
