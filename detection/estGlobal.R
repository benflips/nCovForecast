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
## --------------------------

#packages for reading data and implementing non-negative linear Poisson regression
library("readr")
library("addreg")

#functions for estimation and projection
source("detection/estFunctions.R")

#download cumulative diagnosis counts for all countries/regions from gitHub repository
tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
tsI<-read_csv(file = tsConf)
cases.all <- t(tsI)[-c(1:4),]
T<-dim(cases.all)[1]

#incubation distribution: discretised version of a logNormal with mean 5.2 days and 95% percentile 12.5 days
#Li et al. (2020) NEJM DOI:10.1056/NEJMoa2001316
#maximum incubation period is assumed to be 25 days

inc.dist<-cbind(c(0:25),
c(0.0007824485, 0.0634008850, 0.1604242496, 0.1754661346, 0.1490412449,
0.1153347953, 0.0860250431, 0.0633235540, 0.0465104972, 0.0342707375,
0.0254009386, 0.0189623563, 0.0142655551, 0.0108167723, 0.0082656861,
0.0063641086, 0.0049357206, 0.0038546201, 0.0030303317, 0.0023973807,
0.0019080376, 0.0015272519, 0.0012290953, 0.0009942508, 0.0008082266,
0.0006600780))

colnames(inc.dist)=c("days","probability")

#Design matrix for linear Poisson regression
designF<-design(T,inc.dist)

#produce cumulative infection estimates for each country/region
infect.total<-apply(cases.all,2,infect.est,inc.dist,designF)
cumulative.infections<-cbind(tsI[,1:4],t(infect.total))
colnames(cumulative.infections)<-colnames(tsI)

#produce cumulative projections and append to cumulative observed cases
projections<-apply(infect.total,2,project,inc.dist,designF) #daily new cases over projection period
projections<-(as.numeric(cases.all[T,]))+t(projections) #convert to cumulative cases over projection period
cumulative.projections<-cbind(tsI,projections)

#save output 
cumulative<-list(infections=cumulative.infections,projections=cumulative.projections)
save(cumulative,file="estGlobal.RData")

