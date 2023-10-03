# this script runs the ABM by sourcing functions from other scripts

#=======================================================================
# directory stuff
rm(list=ls())
wd<-getwd()
base.directory<-wd
outdir<-paste0(base.directory, "/output/")
# load all functions
source(paste0(base.directory, "/source/function.sourcer.R"))
# show parameters
data.frame(unlist(parameters))

#=======================================================================

# generate lakes randomly placed on a grid.
lakeLocation<-lake.location(parameters)
# place anglers on a grid
anglerLocation<-angler.location(parameters)

# working list that will go into the loop
fishery<-list(lakeLocation, anglerLocation)
output<-list()
