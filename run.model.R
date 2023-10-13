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
#set.seed(992)
set.seed(73)
# generate lakes randomly placed on a grid.
lakeLocation<-lake.location(parameters)

# place anglers on a grid
anglerCharacteristics<-angler.characteristics(parameters)

# find distances between anglers and lakes
lakeDistance<-lake.distance(lakeLocation, anglerCharacteristics)

# important lake characteristics. This is a placeholder until I set up fish population models.
# Population is drawn from Poisson distribution with lambda=1000, catch is determined by catch equation. Currently no annual reproduction
lakeCharacteristics<-lake.characteristics(parameters)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
anglerDecisions<-create.blank.angler.decisions(parameters)

# make an output object that records how many trips have occurred to each lake annually. Add distribution of travel time? 

# working list that will go into the loop. Each iteration it will be updated 
#with the current fish populations, etc
fishery<-list(lakeLocation=lakeLocation, 
              anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions)

# list that will hold important output from each loop
lakeStatus<-initialize.output.lakes(parameters, lakeCharacteristics)

output<-list(lakeStatus=lakeStatus)

annualOutput<-initialize.annual.output(parameters, lakeCharacteristics)

#annualOutput<-list(annualExploitation=annualExploitation)

# adding outer year loop--will add natural fish population changes (M, r)

for(y in 1:parameters[["nYears"]]){

for(t in 1:parameters[["nDays"]]){
  
  fishery<-angler.decisions(fishery) # each angler chooses a lake. These decisions are added to the anglerLocation df
  
  fishery<-fishing(fishery, parameters) # anglers catch fish and lake populations are updated
  

  output<-output.script(fishery, t, y, output, parameters)
  
}
  # this is where the fish population will be updated annually with recruitment
  annualOutput<-annual.exploitation(y, parameters, output, annualOutput)
  
}

# add final output and plotting step
# also need to add storage of table outputs

plots<-plotting.lake.status(output, fishery, parameters, lakeLocation, anglerCharacteristics)

plots
ggsave(paste0(wd, "/output", "/sim.v1.figure.png"))



