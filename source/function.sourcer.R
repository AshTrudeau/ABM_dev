# this holds the directories of all the functions for the ABM
# Set working directory, import packages, source functions

setwd(paste0(base.directory, "/source/"))
source(paste(get.wd(), "/lake.location.R"))
source(paste(get.wd(), "/parameters.R"))
source(paste(get.wd(), "/angler.location.R"))