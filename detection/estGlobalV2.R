## ---------------------------
##
## Script name: estGlobal.R
##
## Purpose of script: estimation of cumulative infections and projections of new cases based on daily case counts
##
## Author: Ian Marschner
##
## Date Created: 2020-03-30
##
## Email: ian.marschner@ctc.usyd.edu.au
##
## ---------------------------
## ---------------------------
##
## Notes:
##   
## Objects produced by this code:
## 1. cumulative.infections = estimated cumulative number of infections at each day (both diagnosed and undiagnosed) 
## 2. cumulative.projections = forecast cumulative number of new cases for future days, appended to past observed cases 
##
## Currently the forecasting of new cases is disabled in this script 
## --------------------------

#packages for reading data and implementing non-negative linear Poisson regression
library("addreg")

#functions for estimation and projection
source("detection/estFunctionsV3.R")
source("functions.R")

orgLevel <- commandArgs()[6] # get relevant command line argument

load(paste0("dat/",orgLevel,"/cacheData.RData"))
cases.all <- t(timeSeriesInfections)[-1,]

                            
# organise case data by country

T<-dim(cases.all)[1]

#incubation distribution: discretised version of a logNormal(mean=5.2 days, 95% percentile=12.5 days)
#Li et al. (2020) NEJM DOI:10.1056/NEJMoa2001316
#maximum incubation period is assumed to be 25 days

inc.dist<-cbind(c(0:25),
                c(0.0167216581, 0.1209939502, 0.1764151755, 0.1645457302, 0.1319862874, 0.0998480498,
                  0.0738430769, 0.0542463417, 0.0398885964, 0.0294712744, 0.0219198648, 0.0164263561,
                  0.0124063530, 0.0094438368, 0.0072440676, 0.0055980247, 0.0043568666, 0.0034139852,
                  0.0026925052, 0.0021365915, 0.0017053915, 0.0013687946, 0.0011044472, 0.0008956368,
                  0.0007297832, 0.0005973546))

colnames(inc.dist)=c("days","probability")

#Design matrix for linear Poisson regression
designF<-design(T,inc.dist)

#produce cumulative infection estimates for each country/region
infect.total<-apply(cases.all,2,infect.est,inc.dist,designF)
cumulative.infections<-data.frame(timeSeriesInfections[,1],t(infect.total))
colnames(cumulative.infections)<-colnames(timeSeriesInfections)

#Produce infection projections 
projectTo <- 5 # days to project forward
projections<-apply(infect.total, 2, project, inc.dist, designF, proj.days = projectTo, inf.extrap = 7) #daily new cases over projection period
projections<-(as.numeric(cases.all[T,]))+t(projections) #convert to cumulative cases over projection period
cumulative.projections<-data.frame(timeSeriesInfections[,1], projections) # form dataframe
colnames(cumulative.projections) <- c("Region", format(dates[length(dates)]+1:5, "%m.%d.%y"))
cumulative.projections <- cbind(cumulative.infections, cumulative.projections[,-1])

# produce death projections
infMat <- as.matrix(timeSeriesInfections[, -1])
newCasesMat <- infMat - cbind(rep(0, nrow(infMat)), infMat[,-ncol(infMat)])
deathProj <- round(newCasesMat[, (ncol(newCasesMat)-(17+projectTo-1)):(ncol(newCasesMat)-17)]*0.025, 0)
deathProj <- t(apply(deathProj, 1, cumsum))+timeSeriesDeaths[,ncol(timeSeriesDeaths)]
death.projections <- data.frame(timeSeriesDeaths, deathProj)
colnames(death.projections) <- colnames(cumulative.projections)

# estimate best time to recovery, given recovery data, for each region
recTime <- rep(NA, nrow(timeSeriesInfections))
for (rr in 1:length(recTime)){
  recTime[rr]<-recLagOptim(unlist(timeSeriesInfections[rr, -1]),
                           unlist(timeSeriesDeaths[rr, -1]),
                           unlist(timeSeriesRecoveries[rr, -1]))
}

# generate active cases given variable ttr
active.cases <- recLag(cumulative.infections, timeSeriesDeaths, ttr = recTime)
colnames(active.cases)<- colnames(cumulative.infections)

# produce projected active cases
  ## TO DO

#save output 
save(cumulative.infections, active.cases, cumulative.projections, file=paste0("dat/",orgLevel,"/estDeconv.RData"))

