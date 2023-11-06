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
# dropping these because they are specific to the delay difference model

# lakeClasses.c.rho.df<-read_csv(here::here(base.directory,"data","walleye.lakeClass.length.weight.lm.csv"))%>%
#   rename("lakeClasses"="LakeClass")
# lakeClasses.length.weight<-read_csv(here::here(base.directory, "data", "walleye.lakeClass.length.weight.csv"))

allLakes<-read_csv(here::here("data","all.lake.classes.wi.csv"))


# show parameters
data.frame(unlist(parameters))

#=======================================================================
#set.seed(992)
set.seed(73)

# replace this with a 'lake selection' script later
# temporary, the one lake I'm looking at for now

lakes<-filter(allLakes, WBIC==1631900)


# generate lakes randomly placed on a grid.
lakeLocation<-lake.location(parameters, lakes)

# place anglers on a grid
anglerCharacteristics<-angler.characteristics(parameters)

# find distances between anglers and lakes
lakeDistance<-lake.distance(lakeLocation, anglerCharacteristics)

# important lake characteristics. This pulls an age length key
lakeCharacteristics<-lake.characteristics(lakes, parameters)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
anglerDecisions<-create.blank.angler.decisions(parameters)

# make an output object that records how many trips have occurred to each lake annually. Add distribution of travel time? 

# working list that will go into the loop. Each iteration it will be updated 
#with the current fish populations, etc

lakeStatus<-initialize.output.lakes(parameters, lakeCharacteristics)

# fish population matrix. This will eventually need to accommodate multiple lakes; make it a list?
# yes, when I have multiple lakes, make this into another list of matrices, 1 for each lake
fishPop<-initialize.fish.pop(parameters, lakeCharacteristics)

# make selectivity vector
selectivity<-initialize.selectivity(parameters)

# make FmortAge matrix--will hold the number of fish of each age class harvested each year
# when I have multiple lakes, this will need to be a list of matrices, 1 for each lake
FmortAge<-initialize.FmortAge(parameters)

# make NmortAge matrix--will hold the number of fish of each age class that died naturally each year
# when I have multiple lakes, this will need to be a list of matrices, 1 for each lake
NmortAge<-initialize.NmortAge(parameters)


# list that will hold important output from each daily loop

fishery<-list(lakeLocation=lakeLocation, 
              anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions,
              lakeStatus=lakeStatus,
              fishPop=fishPop,
              selectivity=selectivity,
              FmortAge=FmortAge,
              NmortAge=NmortAge)


# df (maybe list later) holding important annual output
annualOutput<-initialize.annual.output(parameters, fishery)


# adding outer year loop--will add natural fish population changes (M, r)

for(y in 1:parameters[["nYears"]]){
  # temporary while working on the annual loop
  y<-1
  #t<-1

for(t in 1:parameters[["nDays"]]){
  
  fishery<-angler.decisions(fishery, t, y) # each angler chooses a lake. These decisions are added to the anglerDecisions
  
  fishery<-fishing(fishery, parameters, t, y) # anglers catch fish and lake populations are updated
  
}
  # this is where the fish population will be updated annually 
  # exploitation and survival
  
  # this is where I left off. Work with the fishPop matrix in the fishery list to add: 
    # harvest of age classes (vulnerability after year x?)
      # actually, probably need to make a vulnerability matrix; depending on growth rate in the scenario, what is the age of 
      # vulnerability for that lake's walleye population? 
    # natural mortality
    # ageing (move population up one year in age)
    # recruitment
  
  # once running, add growth and biomass
  
  annualOutput<-annual.exploitation(y, parameters, fishery, annualOutput)
  # recruitment
  annualOutput<-recruitment(y, parameters, fishery, annualOutput)
  # change in fish N and B for next year. Update fishery status 
  annualOutput<-update.pop(y, parameters, fishery, annualOutput)
  
  fishery<-update.lakes(y, parameters, fishery, annualOutput)
  
}

# add final output and plotting step
# also need to add storage of table outputs

# next fix plots
plots<-plotting.lake.status(annualOutput, fishery, parameters)

plots
ggsave(paste0(wd, "/output", "/sim.v2.figure.png"), height=6, width=8)



