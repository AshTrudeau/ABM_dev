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


# read in some data needed for running population models
lakeClasses.c.rho.df<-read_csv(here::here(base.directory,"data","walleye.lakeClass.length.weight.lm.csv"))%>%
  rename("lakeClasses"="LakeClass")
lakeClasses.length.weight<-read_csv(here::here(base.directory, "data", "walleye.lakeClass.length.weight.csv"))

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
lakeCharacteristics<-lake.characteristics(parameters, lakeClasses.c.rho.df, lakeClasses.length.weight)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
anglerDecisions<-create.blank.angler.decisions(parameters)

# make an output object that records how many trips have occurred to each lake annually. Add distribution of travel time? 

# working list that will go into the loop. Each iteration it will be updated 
#with the current fish populations, etc

lakeStatus<-initialize.output.lakes(parameters, lakeCharacteristics)

# list that will hold important output from each daily loop

fishery<-list(lakeLocation=lakeLocation, 
              anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions,
              lakeStatus=lakeStatus)

# df (maybe list later) holding important annual output
annualOutput<-initialize.annual.output(parameters, lakeCharacteristics)


# adding outer year loop--will add natural fish population changes (M, r)

for(y in 1:parameters[["nYears"]]){

for(t in 1:parameters[["nDays"]]){
  
  fishery<-angler.decisions(fishery, t, y) # each angler chooses a lake. These decisions are added to the anglerDecisions
  
  fishery<-fishing(fishery, parameters, t, y) # anglers catch fish and lake populations are updated
  
}
  # this is where the fish population will be updated annually 
  # exploitation and survival
  annualOutput<-annual.exploitation(y, parameters, fishery, annualOutput)
  # recruitment
  annualOutput<-recruitment(y, parameters, fishery, annualOutput)
  # change in fish N and B for next year. Update fishery status (separate script?)
  
}

# add final output and plotting step
# also need to add storage of table outputs

plots<-plotting.lake.status(output, fishery, parameters, lakeLocation, anglerCharacteristics)

plots
ggsave(paste0(wd, "/output", "/sim.v1.figure.png"))



