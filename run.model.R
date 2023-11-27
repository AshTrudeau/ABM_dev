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
data.frame(unlist(parameters))

#----------------------------------------------------------------------

set.seed(382)

#drawProp<-read_csv(here::here("data","complex.lake.proportion.csv"))
drawProp<-read_csv(paste0(base.directory, "/", "data/", "complex.lake.proportion.csv"))

# lake specific VBGF params from Paul (all structures). not sure why there are multiple years per lake; i thought they were grouped.
# ask after thanksgiving. For now I'll just use the latest params
# keeping this code in 'run.model' temporarily until I replace it with HUC-specific values

vbgf_lakeSpecific<-read_csv(paste0(base.directory, "/", "data/", "vbgf_params.csv"))%>%
  filter(species=="walleye" & state=="wi" & level=="lake.id")%>%
  rename("WBIC"=lake.id)%>%
  # remove some rows with extremely high linf
  filter(linf<100)%>%
  group_by(WBIC)%>%
  slice(which.max(year))%>%
  ungroup()%>%
  select(WBIC, linf, k, t0)


# note that units for length are mm in this dataset but cm in Paul's lake-specific dataset
vbgf_lakeClass<-read_csv(paste0(base.directory, "/", "data/", "lake class standards/", "Lake Class Standards Von Bert.csv"))%>%
  filter(Species=="Walleye" & Percentile=="0.50_percentile")%>%
  select(LakeClass, vb_param, value)%>%
  pivot_wider(names_from=vb_param,
              values_from=value)%>%
  rename("lakeClass"=LakeClass,
         "linf"=Linf,
         "k"=K)%>%
  # convert to cm
  mutate(linf=linf/10,
         t0=t0/10)

allLakes<-read_csv(paste0(base.directory, "/", "data/", "all.lake.classes.wi.csv"))%>%
  rename("lakeClass"=`Final Lake Class`)%>%
  # sticking with Vilas county before scaling up
  filter(County=="Vilas" & grepl("Complex", lakeClass))%>%
  mutate(lakeClass=str_replace_all(lakeClass, " - ", " "), 
         WBIC=as.character(WBIC))%>%
  mutate(lakeSpecificGrowth=ifelse(WBIC%in%vbgf_lakeSpecific$WBIC, 1, 0))

# 35 lakes have their own VBGF


#  For now selecting only from [possible] walleye lakes (classified as 'complex')


# select lakes

selectLakes<-select.lakes(parameters, allLakes, drawProp)

# attach growth parameters to lake table; specific if we have it, or class if not (later watershed)
selectLakes<-growth.params(selectLakes, vbgf_lakeClass, vbgf_lakeSpecific)




# grab the lake locations (might be redundant)
lakeLocation<-lake.location(parameters, selectLakes)

# place anglers on a grid. eventually add other characteristics besides location 
anglerCharacteristics<-angler.characteristics(parameters)

# find straight-line distances between anglers and lakes. Eventually this can use Gmaps API to find actual
# travel times, but that will cost money.
lakeDistance<-lake.distance(lakeLocation, anglerCharacteristics)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
# in this version, anglerDecisions is backburnered because there is only 1 lake
anglerDecisions<-create.blank.angler.decisions(parameters)

# make an output object that records how many trips have occurred to each lake annually. Add distribution of travel time? 

# working list that will go into the loop. Each iteration it will be updated 
#with the current fish populations, etc

lakeStatus<-initialize.output.lakes(parameters, selectLakes)

# fish population matrix. 
# this is now a nested list with 1 matrix per lake
fishPops<-initialize.fish.pop(parameters, selectLakes)

# make copy to keep track of starting population of each year. in contrast, fishPops is updated each day of each year
# note change to plural
startPops<-initialize.start.pop(parameters)

# make selectivity vector
selectivity<-initialize.selectivity(parameters)

# make a harvest at age object (need this to estimate age specific F later)
harvestAge<-initialize.harvestAge(parameters)

# make FmortAge matrix--will hold the number of fish of each age class harvested each year
# now a list of matrices
FmortAge<-initialize.FmortAge(parameters)

# make NmortAge matrix--will hold the number of fish of each age class that died naturally each year
# now a list of matrices
NmortAge<-initialize.NmortAge(parameters)

# make this script: use VBGF to predict length at age for each waterbody
fishSizes<-fish.size(parameters, selectLakes)


# list that will hold important output from each daily loop

fishery<-list(lakeLocation=lakeLocation, 
              anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
             # lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions,
              lakeStatus=lakeStatus,
              # fishPop will be a nested list, 1 matrix for each lake
              fishPops=fishPops,
              fishSizes=fishSizes,
              startPops=startPops,
              # harvestAge also a nested list
              harvestAge=harvestAge,
              selectivity=selectivity,
              # FmortAge and NmortAge also nested lists
              FmortAge=FmortAge,
              NmortAge=NmortAge)


# df  holding important annual output
annualOutput<-initialize.annual.output(parameters, fishery)


# adding outer year loop--will add natural fish population changes (M, r)

for(y in 1:parameters[["nYears"]]){

  y<-1

for(t in 1:parameters[["nDays"]]){
  #t<-1
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

ggplot(annualOutput)+
  geom_line(aes(x=year, y=fishNEnd))

ggplot(annualOutput)+
  geom_line(aes(x=year, y=fishBEnd))

ggplot(annualOutput)+
  geom_line(aes(x=year, y=annualHarvestN))

ggplot(annualOutput)+
  geom_line(aes(x=year, y=maxSize))

ggplot(annualOutput)+
  geom_line(aes(x=year, y=PSDQuality))

# what did recruitment look like? 

fishStart<-fishery[["startPop"]]

plot(fishStart[1,])


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
