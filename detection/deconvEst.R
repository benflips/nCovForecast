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
## ---------------------------

## load up our functions into memory
source("detection/deconvFunctions.R")
source("functions.R")
## ---------------------------

cat("  Running deconvolutions...\n")

orgLevel <- commandArgs()[6] # get relevant command line argument

# load relevant dataset
load(paste0("dat/",orgLevel,"/cacheData.RData"))
cases.all <- t(timeSeriesInfections)[-1,]

# number of days of data
T<-dim(cases.all)[1]

# Assumed incubation distribution
inc.dist <- incubation()

# back projections
cat("   Back projections...\n")
backProjection <- apply(cases.all, 2, BackProj, dist = inc.dist)

# forward projections
cat("   Forward projections...\n")
forwardProjection <- apply(backProjection, 2, ForwardProj, dist = inc.dist, proj.days=10)
