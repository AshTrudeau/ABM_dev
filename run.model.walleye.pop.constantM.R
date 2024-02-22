# 2/22/24 
# checking walleye population models by comparing to population estimates of naturally
# recruiting, surveyed lakes in WI
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
# At some point, see if I can get lake specific random intercepts for walleye population model that DNR uses

set.seed(9282)

# read in the DNR population estimates (also includes age 0 cpe and fishign effort), select
# only naturally recruiting (NR) populations

pop.est<-read_csv(paste0(base.directory, "/data/","pe.yoy.effort.csv"))%>%
  filter(CODE=="NR")

wbics<-unique(pop.est$wbic)


# for now using lake class; I need to set up a meeting with Paul to troubleshoot HALK params 

vbgf_lakeClass<-read_csv(paste0(base.directory, "/", "data/", "LakeClassStandardsVonBert.csv"))%>%
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

# all of these lakes are NR and have at least one PE on record.
allLakes<-read_csv(paste0(base.directory, "/", "data/", "all.lake.classes.wi.csv"))%>%
  rename("lakeClass"=`Final Lake Class`)%>%
  # filter(County=="Vilas")%>%
  filter(grepl("Complex", lakeClass) & !grepl("Riverine", lakeClass))%>%
  mutate(lakeClass=str_replace_all(lakeClass, " - ", " "), 
         WBIC=as.character(WBIC),
         has.pe=ifelse(WBIC%in%as.character(wbics), 1, 0))%>%
  filter(has.pe==1)%>%
  left_join(vbgf_lakeClass, by="lakeClass")

  
selectLakes<-allLakes

lakeCharacteristics<-lake.characteristics(parameters, selectLakes, vbgf_lakeClass)

# place anglers on a grid. eventually add other characteristics besides location 
anglerCharacteristics<-angler.characteristics(parameters)

# spot check  on locations
# ggplot()+
#   geom_point(data=anglerCharacteristics, aes(x=anglerLong, y=anglerLat))+
#   geom_point(data=lakeCharacteristics, aes(x=lakeLong, lakeLat), color="blue")

# find straight-line distances between anglers and lakes. Eventually this can use Gmaps API to find actual
# travel times, but that will cost money.
lakeDistance<-lake.distance(lakeCharacteristics, anglerCharacteristics)


anglerDecisions<-create.blank.angler.decisions(parameters)

# make an output object that records how many trips have occurred to each lake annually. Add distribution of travel time? 


# fish population matrix. 
# this is now a nested list with 1 matrix per lake
# number of age 0  fish in first step of burn in is N0*lake area
fishPops<-initialize.fish.pop.burnin(parameters, lakeCharacteristics)

# make copy to keep track of starting population of each year. in contrast, fishPops is updated each day of each year
# note change to plural
startPops<-initialize.start.pop.burnin(parameters)

# make selectivity vector
selectivity<-initialize.selectivity(parameters)

# make a harvest at age object (need this to estimate age specific F later)
harvestAge<-initialize.harvestAge(parameters)

# make FmortAge matrix--will hold the number of fish of each age class harvested each year
# now a list of matrices
FmortAge<-initialize.FmortAge(parameters)

# moved ahead of Nmort because Charnov specification requires length at age
# make this script: use VBGF to predict length at age for each waterbody
fishSizes<-fish.size(parameters, lakeCharacteristics)


# make NmortAge matrix--will hold the number of fish of each age class that died naturally each year
# now a list of matrices
NmortAge<-initialize.Nmort.constant(parameters, fishSizes, selectLakes)


# list that will hold important output from each daily loop

fishery<-list(anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions,
              # moved lakeStatus initialization down; hold place in fishery list
              lakeStatus=NA,
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




# burn in fish populations--50 years worked well for starting age 0 population of 5000 fish
#for(y in 1:nBurnIn){
for(y in 1:nBurnIn){
  # burnin=T will skip F mort calculation
  

  fishery<-natural.mortality.constant(y, fishery, parameters)
  fishery<-ageing(y, fishery, parameters, burnin=TRUE)
  fishery<-recruitment(y, fishery, parameters, burnin=TRUE)
  fishery<-update.fishPops(y, fishery, parameters, burnin=TRUE)
  print(y)
}

# compare these starting populations with PEs--I would expect these to be a lot higher
# add up n of all age classes in the final year for each lake
#  test.start<-lapply(fishery[["fishPops"]], function(x) sum(x[,30]))
#  test.start.df<-data.frame(wbic=names(test.start), population=unlist(test.start))
# # 
# # # get minimum year from pop.est
# # 
#  pop.est.early<-pop.est%>%
#    mutate(wbic=as.character(wbic))%>%
#    group_by(wbic)%>%
#    slice(which.min(year))%>%
#    ungroup()%>%
#    select(wbic, year, pe, AREA)%>%
#    rename("pe.real"=pe)%>%
#    left_join(test.start.df, by="wbic")%>%
#    rename("pop.sim"=population)
#  
#  ggplot(pop.est.early)+
#    geom_point(aes(x=pe.real, y=pop.sim))+
#    geom_abline(slope=1, intercept=0)+
#    theme_bw()
#  
#  pop.est.pivot<-pop.est.early%>%
#    pivot_longer(cols=c("pe.real","pop.sim"), names_to="pe.type", values_to="pe")
#  
#  ggplot(pop.est.pivot)+
#    geom_point(aes(x=AREA, y=pe))+
#    facet_grid(pe.type~.)
#  
#  cor(pop.est.early$pe.real, pop.est.early$pop.sim, use="complete.obs")
#  mod<-lm(pe.real~pop.sim, data=pop.est.early)
#  summary(mod)
# r2 of 0.89, correlation of 0.94. lake size alone does an okay job, but there's room for improvement. 
# leave it for now, but it's a place to adjust later. 

 # some of the larger lakes have closer simulated estimates with constant M, but there may be more scattering at the smaller
 # lakes. 
 
# update startPops with equilibrium fish population 
# startPops is the *starting* population of the final year of burn-in. 
# (new) fishPops is ALSO the starting population of the final year of burn-in. 
# (reason: recruitment does not take place during final year)
fishery<-initialize.start.pop(parameters, fishery)

fishery<-initialize.fish.pop(parameters, lakeCharacteristics, fishery)

# update fishPops and startPops with equiliibrium fish population 


# Each iteration (day year), lakeStatus will be updated with the current fish populations
# to initialize, add up nFish in each lake for start of simulation (day 0 year 0)
# this is where I left off to calm down
fishery<-initialize.lake.status(parameters, lakeCharacteristics, fishery)


# df  holding important annual output
annualOutput<-initialize.annual.output(parameters, fishery)

# simulation now starts with equilibrium fish populations
# next update this code for different fishPops structure
for(y in 1:parameters[["nYears"]]){
  #y<-1
  
  for(t in 1:parameters[["nDays"]]){
    #t<-1
    fishery<-angler.decisions(fishery, t, y) # each angler chooses a lake. These decisions are added to the anglerDecisions
    
    fishery<-fishing(fishery, parameters, t, y) # anglers catch fish and lake populations are updated
    print(t)
    
  }
  
  # calculate fishing mortality by age
  fishery<-fishing.mortality(y,  fishery)
  
  # apply  natural mortality by age to fishPops (adjusting for F)
  fishery<-natural.mortality.constant(y, fishery, parameters)
  
  
  # apply ageing
  fishery<-ageing(y, fishery, parameters, burnin=FALSE)
  
  # left off here 11/29/23; update recruitment next
  # apply recruitment
  fishery<-recruitment(y, fishery, parameters, burnin=FALSE)
  
  
  # close loop, store annual output
  
  annualOutput<-annual.output(y, fishery, annualOutput, parameters)
  
  # update lakeStatus object to start next year's loop
  fishery<-update.lakes(y, fishery, parameters)
  
  print(y)
  
}


# visualizing outputs

# writing out outputs to working directory. I think these should go in the  'output' 
# created in the job script

print(annualOutput)
print(base.directory)



write.csv(annualOutput, paste0(base.directory,"/output/annual.output.constantM.csv"))
# annualOutput<-read_csv(here::here("output","annual.output.csv"))
# lakeStatus<-read_csv(here::here("output","lake.status.csv"))



lakeStatus<-fishery[["lakeStatus"]]

print(lakeStatus)

write.csv(lakeStatus, paste0(base.directory,"/output/lake.status.constantM.csv"))

