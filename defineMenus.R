## ---------------------------
##
## Script name: defineMenus.R
##
## Purpose of script: To define variables for drop-down menus
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
## Is called after getData.R
## --------------------------
## load up the packages we will need 
##

# get region names and cases as of yesterday
regSum <- tapply(tsA[[ncol(tsI)-1]], INDEX = list(tsA$Country.Region), FUN = sum)
regSum <- regSum[rev(order(regSum))]
regSum <-regSum[regSum>9] # remove regions with fewer than ten active cases

ddNames <- names(regSum)
ddReg <- ddNames
names(ddReg) <- paste0(ddNames, " (", regSum, ")")
#ddReg <- paste(ddReg, collapse = ", ") # menu specifier

