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
## load up the packages we will need 
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("addreg"))
suppressPackageStartupMessages(library("turboEM", quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library("SparseM", quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library("gam"))
suppressPackageStartupMessages(library("parallel"))
## ---------------------------

## load up our functions into memory
source("detection/deconvFunctions.R")
source("functions.R")
## ---------------------------

orgLevel <- commandArgs()[6] # get relevant command line argument

cat("  Running deconvolutions for ", orgLevel, " using new method...\n")

# load relevant dataset
load(paste0("dat/",orgLevel,"/cacheData.RData"))
cases.all <- t(timeSeriesInfections[,-1])
cases.all <- as.data.frame(cases.all)
names(cases.all) <- timeSeriesInfections[,1]
# number of days of data
T<-dim(cases.all)[1]
# Number of regions
R <- dim(cases.all)[2]

# Assumed incubation distribution
inc.dist <- incubation()

# Make a cluster for parallelising the fitting task
# Calculate the number of cores
nCores <- 2
# Initiate cluster
cl <- makeCluster(nCores)

# back projections
cat("   Back projections...\n")
backProjection <- parLapply(cl, cases.all, BackProj, dist = inc.dist)

# stop cluster
stopCluster(cl)

# extract estimated new infections by time
infect.total <- matrix(NA, nrow = T, ncol = R)
for (rr in 1:R){
  infect.total[,rr] <- backProjection[[rr]][,"infections"]
}

# express as cumulant and in JHU layout
cumulative.infections <- apply(infect.total, 2, cumsum)
# get undiagnosed cases
undiagnosed.infections <- cumulative.infections - t(timeSeriesInfections[,-1])
#express both in JHU layout
cumulative.infections<-data.frame(timeSeriesInfections[,1],t(cumulative.infections))
undiagnosed.infections<-data.frame(timeSeriesInfections[,1],t(undiagnosed.infections))

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
