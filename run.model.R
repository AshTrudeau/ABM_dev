# 10/3/2023
# this script runs the ABM by sourcing functions from other scripts

#=======================================================================
# directory stuff
rm(list=ls())
wd<-"C:/Users/ashle/Dropbox/bluegill management postdoc/lakeSelectTool/ABM_dev"
base.directory<-wd
outdir<-paste0(base.directory, "/output/")
# load all functions
source(paste0(base.directory, "/source/function.sourcer.R"))
# show parameters
data.frame(unlist(parameters))

#=======================================================================
set.seed(992)
# generate lakes randomly placed on a grid.
lakeLocation<-lake.location(parameters)

# place anglers on a grid
anglerCharacteristics<-angler.characteristics(parameters)

# find distances between anglers and lakes
lakeDistance<-lake.distance(lakeLocation, anglerCharacteristics)

# important lake characteristics. This is a placeholder until I set up fish population models.
# lambda is the Poisson coefficient for drawing catch (harvest and catch are identical for now)
lakeCharacteristics<-lake.lambda(parameters)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
anglerDecisions<-create.blank.angler.decisions(parameters)


# working list that will go into the loop. Each iteration it will be updated 
#with the current fish populations, etc
fishery<-list(lakeLocation=lakeLocation, 
              anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions)
# list that will hold important output from each loop
output<-list()


for(t in 1:parameters[["nDays"]]){
  fishery<-angler.decisions(fishery) # each angler chooses a lake. These decisions are added to the anglerLocation df
  
  fishery<-fishing(fishery, parameters) # anglers catch fish and lake populations are updated
  
  output<-outputScript(fishery, t, output)
  
}
