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
# Population is drawn from Poisson distribution with lambda=1000, catch is determined by catch equation. Currently no annual reproduction
lakeCharacteristics<-lake.characteristics(parameters)

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
lakeStatus<-initialize.output.lakes(parameters, lakeCharacteristics)

output<-list(lakeStatus=lakeStatus)

# adding outer year loop--will add natural fish population changes (M, r)

for(y in 1:parameters[["nYears"]]){

for(t in 1:parameters[["nDays"]]){
  
  fishery<-angler.decisions(fishery) # each angler chooses a lake. These decisions are added to the anglerLocation df
  
  # problem with this function. runs only for the first loop. 
  fishery<-fishing(fishery, parameters) # anglers catch fish and lake populations are updated
  
  # replace 1 with t for loop
  output<-output.script(fishery, t, y, output, parameters)
  
}
  # this is where the fish population will be updated annually
  
}

# add final output and plotting step

lakeStatus<-output[["lakeStatus"]]

plots<-plotting.lake.status(lakeStatus, fishery)

# aggregate by year, then make this into another function

agg.year<-lakeStatus%>%
  group_by(year, lakeID)%>%
  summarize(totalHarvest=sum(nHarvested),
            totalEffort=sum(nAnglers),
            fishPop=min(fishPop))%>%
  ungroup()

# somthing is wrong with the fishing effort count

ggplot(agg.year)+
  geom_line(aes(x=year, y=fishPop, color=as.factor(lakeID)))

ggplot(agg.year)+
  geom_line(aes(x=year, y=nAnglers, color=as.factor(lakeID)))

ggplot(data=lakeStatus)+
  geom_line(aes(x=day, y=fishPop, color=as.factor(lakeID)))


