## ---------------------------
##
## Script name: deconvEst.R
##
## Purpose of script: Run Ian Marschner's back-projection model on a JHU-style dataframe
##
## Author: Ben Phillips (following example script provided by Ian)
##
## Date Created: 2020-06-22
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes: Designed to be run from source as a stand alone script
##   
##
## --------------------------
# Make a cluster for parallelising the fitting task

## load up the packages we will need 
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("addreg"))
suppressPackageStartupMessages(library("turboEM", quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library("SparseM", quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library("gam"))
#suppressPackageStartupMessages(library("parallel"))
## ---------------------------
# # Calculate the number of cores
# nCores <- 2
# # Initiate cluster
# cl <- makeCluster(nCores)

## load up our functions into memory
source("detection/deconvFunctions.R")
source("functions.R")
## ---------------------------

orgLevel <- commandArgs()[6] # get relevant command line argument
short.arg <- commandArgs()[7] # get relevant command line argument
if( (!is.na(short.arg))&(tolower(short.arg) == "short") ) {
  run.short <- TRUE
} else{
  run.short <- FALSE
}

if( run.short ){
  cat("  Only running deconvolutions for last 4 months\n")
  short.len <- 31*4
  nTries <- 10
  nGrow <- 5
}
cat("  Running deconvolutions for", orgLevel, "using new method...\n")

# load relevant dataset
load(paste0("dat/",orgLevel,"/cacheData.RData"))
init.total <- rep(0,dim(timeSeriesInfections)[1])
if( run.short ) {
  if (file.exists(paste0("dat/",orgLevel,"/estDeconv.RData"))) {
    load(paste0("dat/",orgLevel,"/estDeconv.RData"))
    if (dim(timeSeriesInfections)[1] != dim(cumulative.infections)[1]) {
      #special case for re-calculating when No. of regions changes.
      T.start <- 2
      T.end <- T.start + short.len
    } else {
      last_date <- NA
      last_ind <- dim(cumulative.infections)[2]
      while( (is.na(last_date) & (last_ind>1)) ) {
        last_date <- colnames(cumulative.infections)[last_ind]
        last_ind <- last_ind - 1
      }
      T.start <- which( colnames(timeSeriesInfections) == last_date )[1]
      if ((T.start+short.len) > (dim(timeSeriesInfections)[2])) {
        T.end <- dim(timeSeriesInfections)[2]
	last_ind <- last_ind - (T.start-(T.end-short.len))
	T.start <- max( 2, T.end-short.len )
      } else {
        T.end <- T.start+short.len
      }
      if (T.start > 2) {
        init.total <- cumulative.infections[,last_ind]
      }
    }
  } else {
    T.start <- 2
    T.end <- T.start + short.len
  }
} else {
  T.start <- 2
}

cases.all <- t(timeSeriesInfections[,-1])
# Number of regions
R <- dim(cases.all)[2]
# need to add extra element to start of cases, to calc day 1 new infections
cases.all <- rbind( rep(0,R), cases.all )
cases.all <- as.data.frame(cases.all)
names(cases.all) <- timeSeriesInfections[,1]
# number of days of data (minus extra leading day)
if( run.short ) {
  T <- T.end - T.start + 1
} else {
  T <- dim(cases.all)[1] - 1
}

# Assumed incubation distribution
inc.dist <- incubation()

# back projections
cat("   Back projections...\n")
#backProjection <- lapply(cases.all, BackProj, dist = inc.dist)
backProjection <- as.list(1:R)
#for (rr in 1:R){
#  bp <- BackProj( cases.all[,rr], inc.dist )
#  backProjection[[rr]] <- bp
#}
for (rr in 1:R){
  if( run.short ) {
    bp.len <- short.len
    for (i in 1:nTries) {
      R.start <- T.start
      R.end <- min( R.start+bp.len, dim(timeSeriesInfections)[2] )
      R.start <- max( 2, R.end-bp.len )
      cases.region <- cases.all[(R.start-1):R.end,rr]
      bp <- BackProj( cases.region, inc.dist )
      if (any(is.na( bp[,"infections"] ))) {
        bp.len <- bp.len + nGrow
      } else {
        break
      }
    }
    bp <- bp[(T.start-R.start+1):(T.start-R.start+T),]
  } else {
    bp <- BackProj( cases.all[,rr], inc.dist )
  }
  backProjection[[rr]] <- bp
}

# stop cluster
#stopCluster(cl)

# extract estimated new infections by time
infect.total <- matrix(NA, nrow = T, ncol = R)
for (rr in 1:R){
  src_vec <- backProjection[[rr]][,"infections"]
  if (any(is.na(src_vec))) {
    src_vec[] <- NA
  } else {
    src_vec[1] <- src_vec[1] + init.total[rr]
  }
  infect.total[,rr] <- src_vec
}

# express as cumulant and in JHU layout
calc.cumsum <- apply(infect.total, 2, cumsum)
if( run.short ) {
  if (T.start > 2) {
    prev.cumulative <- t(cumulative.infections[,2:T.start])
    cumulative.infections <- rbind( prev.cumulative, calc.cumsum )
  } else {
    cumulative.infections <- calc.cumsum
  }
  T.start <- T.end - dim(cumulative.infections)[1] + 1
} else {
  cumulative.infections <- calc.cumsum
}

# get undiagnosed cases
if( run.short ) {
  undiagnosed.infections <- cumulative.infections - t(timeSeriesInfections[,T.start:T.end])
} else {
  undiagnosed.infections <- cumulative.infections - t(timeSeriesInfections[,-1])
}

#express both in JHU layout
cumulative.infections<-data.frame(timeSeriesInfections[,1],t(cumulative.infections))
if( run.short ) {
  c.names <- colnames(timeSeriesInfections[,T.start:T.end])
  c.names[1] <- colnames(timeSeriesInfections)[1]
  colnames(cumulative.infections)<-c.names
} else {
  colnames(cumulative.infections)<-colnames(timeSeriesInfections)
}
undiagnosed.infections<-data.frame(timeSeriesInfections[,1],t(undiagnosed.infections))
if( run.short ) {
  c.names <- colnames(timeSeriesInfections[,T.start:T.end])
  c.names[1] <- colnames(timeSeriesInfections)[1]
  colnames(undiagnosed.infections)<-c.names
} else {
  colnames(undiagnosed.infections)<-colnames(timeSeriesInfections)
}

# forward projections
cat("   Forward projections...\n")
projectTo <- 5 # days to project forward
forwardProjection <- lapply(backProjection, ForwardProj, dist = inc.dist, proj.days=projectTo)
proj.total <- matrix(NA, nrow = projectTo, ncol = R)
for (rr in 1:R){
  proj.total[,rr] <- forwardProjection[[rr]][,"forecast"]
}
proj.total <- data.frame(timeSeriesInfections[,1],t(proj.total)) # number of new cases on each day into the future

save(cumulative.infections, undiagnosed.infections, proj.total, file=paste0("dat/",orgLevel,"/estDeconv.RData"))
cat("  Deconvolution complete.\n\n")
