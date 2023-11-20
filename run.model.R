# 10/3/2023
# this script runs the ABM by sourcing functions from other scripts

#=======================================================================
# directory stuff
rm(list=ls())
setwd("C:/Users/ashle/Dropbox/bluegill management postdoc/lakeSelectTool/ABM_dev/")
wd<-getwd()
base.directory<-wd
outdir<-paste0(base.directory, "/output/")
# load all functions
source(paste0(base.directory, "/source/function.sourcer.R"))



allLakes<-read_csv(here::here("data","all.lake.classes.wi.csv"))

vbgf_all<-read_csv(here::here("data","vbgf_params.csv"))

# I'm not sure why multiple lake years are present in this 

# try just scale data; maybe all structures were mixed together as separate fits withotu indexing?
# ask Paul after thanksgiving. For now just take the most recent one
vbgfWalleye_lakeID<-vbgf_all%>%
  filter(species=="walleye" & state=="wi" & level=="lake.id")%>%
  group_by(lake.id)%>%
  slice(which.max(year))%>%
  ungroup()

allLakes$vbgf<-ifelse(as.character(allLakes$WBIC)%in%vbgfWalleye_lakeID$lake.id, 1, 0)
# for now
allLakes.vbgf<-filter(allLakes, vbgf==1)


data.frame(unlist(parameters))

#=======================================================================
#set.seed(992)
set.seed(73)

# replace this with a 'lake selection' script later
# temporary, the one lake I'm looking at for now

lakes<-filter(allLakes, WBIC==1545600)
vbgf<-filter(vbgfWalleye_lakeID, lake.id=="1545600")

# generate lakes randomly placed on a grid.
lakeLocation<-lake.location(parameters, lakes)

# place anglers on a grid
anglerCharacteristics<-angler.characteristics(parameters)

# find distances between anglers and lakes
lakeDistance<-lake.distance(lakeLocation, anglerCharacteristics)

# important lake characteristics. This pulls an age length key
lakeCharacteristics<-lake.characteristics(lakes, parameters)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
# in this version, anglerDecisions is backburnered because there is only 1 lake
anglerDecisions<-create.blank.angler.decisions(parameters)

# make an output object that records how many trips have occurred to each lake annually. Add distribution of travel time? 

# working list that will go into the loop. Each iteration it will be updated 
#with the current fish populations, etc

lakeStatus<-initialize.output.lakes(parameters, lakeCharacteristics)

# fish population matrix. This will eventually need to accommodate multiple lakes; make it a list?
# yes, when I have multiple lakes, make this into another list of matrices, 1 for each lake
fishPop<-initialize.fish.pop(parameters, lakeCharacteristics)

startPop<-initialize.start.pop(parameters)

# make selectivity vector
selectivity<-initialize.selectivity(parameters)

# make a harvest at age object (need this to estimate age specific F later)
harvestAge<-initialize.harvestAge(parameters)

# make FmortAge matrix--will hold the number of fish of each age class harvested each year
# when I have multiple lakes, this will need to be a list of matrices, 1 for each lake
FmortAge<-initialize.FmortAge(parameters)

# make NmortAge matrix--will hold the number of fish of each age class that died naturally each year
# when I have multiple lakes, this will need to be a list of matrices, 1 for each lake
NmortAge<-initialize.NmortAge(parameters)

# make this script: use VBGF to predict length at age for each waterbody
fishSize<-fish.size(parameters, vbgf)


# list that will hold important output from each daily loop

fishery<-list(lakeLocation=lakeLocation, 
              anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions,
              lakeStatus=lakeStatus,
              # fishPop will be a nested list, 1 matrix for each lake
              fishPop=fishPop,
              fishSize=fishSize,
              startPop=startPop,
              # harvestAge also a nested list
              harvestAge=harvestAge,
              selectivity=selectivity,
              # FmortAge and NmortAge also nested lists
              FmortAge=FmortAge,
              NmortAge=NmortAge)


# df (maybe list later) holding important annual output
annualOutput<-initialize.annual.output(parameters, fishery)


# adding outer year loop--will add natural fish population changes (M, r)

for(y in 1:parameters[["nYears"]]){

  y<-10

for(t in 1:parameters[["nDays"]]){
  fishery<-angler.decisions(fishery, t, y) # each angler chooses a lake. These decisions are added to the anglerDecisions
  
  fishery<-fishing(fishery, parameters, t, y) # anglers catch fish and lake populations are updated
  #print(t)
  
}

  # calculate fishing mortality by age
  fishery<-fishing.mortality(y,  fishery)
  
  # apply  natural mortality by age
  fishery<-natural.mortality(y, parameters, fishery)
  
  # apply ageing
  fishery<-ageing(y, fishery, parameters)
  
  # apply recruitment
  fishery<-recruitment(y, fishery, parameters)
  

  # close loop, store annual output
  
  annualOutput<-annual.output(y, fishery, annualOutput, parameters)
  
  # update lakeStatus object to start next year's loop
  fishery<-update.lakes(y, fishery, parameters)
  
  print(y)
  
}

write.csv(annualOutput, "annual.output.csv")
lakeStatus<-fishery[["lakeStatus"]]

write.csv(lakeStatus, "lake.status.csv")

# something is wrong with the FmortAge matrix; it only recorded 0 and Inf. harvestAge looks fine

# add final output and plotting step
# also need to add storage of table outputs

# next fix plots. These are placeholders for now
# plots<-plotting.lake.status(annualOutput, fishery, parameters)
# plots<-plotting.single.lake.status(annualOutput, fishery, parameters)
# 
# plots
# ggsave(paste0(wd, "/output", "/sim.v2.figure.png"), height=6, width=8)
# 
# 
# 
