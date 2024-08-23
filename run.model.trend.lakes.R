# 10/3/2023
# this script runs the ABM by sourcing functions from other scripts
# rewriting file paths to work with CHTC

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

set.seed(38)

drawProp<-read_csv(paste0(base.directory, "/data/complex.lake.proportion.csv"))


# HALK growth parameters take 2
load(paste0(base.directory, "/data/wi_halk_aged_growth_params.RData"))

vbgf_lakeSpecific<-growth_params%>%
  filter(species=="walleye")%>%
  filter(!grepl("unk", county))%>%
  filter(halk.level=="year")%>%
  # arbitrarily narrowing down to fyke net sampling
  filter(sampling.method=="fyke_net")%>%
  # and choose the most recent year
  group_by(lake.id)%>%
  slice(which.max(year))%>%
  ungroup()%>%
  rename("WBIC"=lake.id)%>%
  dplyr::select(WBIC, linf, k, t0)%>%
  # some of these linf values are ridiculous
  filter(linf<80 & t0>-5)

# 93 unique lakes with lake and year-specific VBGF.  No longer restricting to Vilas county (can walk it back later)
# when unreasonable linf and t0 values are removed, only 23 lakes

# for now using lake class for the rest of the growth params

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

allLakes<-read_csv(paste0(base.directory, "/", "data/", "all.lake.classes.wi.csv"))%>%
  rename("lakeClass"=`Final Lake Class`)%>%
  # filter(County=="Vilas")%>%
  filter(grepl("Complex", lakeClass) & !grepl("Riverine", lakeClass))%>%
  mutate(lakeClass=str_replace_all(lakeClass, " - ", " "), 
         WBIC=as.character(WBIC))%>%
  mutate(lakeSpecificGrowth=ifelse(WBIC%in%vbgf_lakeSpecific$WBIC, 1, 0))

#selectLakes<-select.lakes(parameters, allLakes, drawProp)
trendLakes<-as.character(read_rds(paste0(base.directory, "/source/trend.wbics.Rds")))

# specifically pulling trend lakes(many years of survey data)
selectLakes<-allLakes%>%
  filter(WBIC%in%trendLakes)
# no lake specific growth available (yet), so using lake classes

lakeCharacteristics<-lake.characteristics(parameters, selectLakes, vbgf_lakeClass, 
                                          vbgf_lakeSpecific)

# place anglers on a grid. eventually add other characteristics besides location 
anglerCharacteristics<-angler.characteristics(parameters)

# spot check  on locations
# ggplot()+
#   geom_point(data=anglerCharacteristics, aes(x=anglerLong, y=anglerLat))+
#   geom_point(data=lakeCharacteristics, aes(x=lakeLong, lakeLat), color="blue")

# find straight-line distances between anglers and lakes. Eventually this can use Gmaps API to find actual
# travel times, but that will cost money.
lakeDistance<-lake.distance(lakeCharacteristics, anglerCharacteristics)

# at some point, revise decisions to switch to next-nearest lake when previous catch=0. (setting up flexibility for integrating memory)
# in this version, anglerDecisions is backburnered because there is only 1 lake
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

# make NmortAge matrix--will hold the number of fish of each age class that died naturally each year
# now a list of matrices
NmortAge<-initialize.NmortAge(parameters)

# make this script: use VBGF to predict length at age for each waterbody
fishSizes<-fish.size(parameters, lakeCharacteristics)

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
  fishery<-natural.mortality(y, fishery, parameters, burnin=TRUE)
  fishery<-ageing(y, fishery, parameters, burnin=TRUE)
  fishery<-recruitment(y, fishery, parameters, burnin=TRUE)
  fishery<-update.fishPops(y, fishery, parameters, burnin=TRUE)
  print(y)
}

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
  fishery<-natural.mortality(y, fishery, parameters, burnin=FALSE)
  
  
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

write.csv(annualOutput, paste0(base.directory,"/output/annual.output.csv"))
# annualOutput<-read_csv(here::here("output","annual.output.csv"))
# lakeStatus<-read_csv(here::here("output","lake.status.csv"))

# library(RColorBrewer)
# library(cowplot)


lakeStatus<-fishery[["lakeStatus"]]

print(lakeStatus)

write.csv(lakeStatus, paste0(base.directory,"/output/lake.status.csv"))


# # next fix plots. These are placeholders for now
# plots<-plotting.lake.status(annualOutput, lakeStatus, lakeCharacteristics, parameters)
# plots
# ggsave(paste0(base.directory, "/figures/", "trend.lake.status.png"), height=8, width=12)
#plots<-plotting.lake.status(annualOutput, fishery, parameters)
#plots
#  ggsave(paste0(wd, "/output", "/sim.v4.figure.png"), height=6, width=10)
#  
# # this doesn't exist yet
# size.structure<-plotting.size.structure(annualOutput, fishery, parameters) 
# 
# annualOutput<-filter(annualOutput, year<50)
# 
#  meanSize<-ggplot(annualOutput)+
#    geom_line(aes(x=year, y=meanSize, color=as.factor(WBIC)), linewidth=1.5)+
#    scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
#    xlab("Year of simulation")+
#    ylab("Mean walleye length (cm)")+
#    guides(color="none")+
#    theme_bw()
#  
#  maxSize<-ggplot(annualOutput)+
#    geom_line(aes(x=year, y=maxSize, color=as.factor(WBIC)), linewidth=1.5)+
#    scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
#    xlab("Year of simulation")+
#    ylab("Max walleye length (cm)")+
#    guides(color="none")+
#    theme_bw()
#  
#  psd<-ggplot(annualOutput)+
#    geom_line(aes(x=year, y=PSDQuality, color=as.factor(WBIC)), linewidth=1.5)+
#    scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
#    xlab("Year of simulation")+
#    ylab("PSD (Quality size)")+
#    guides(color="none")+
#    theme_bw()
#  
#  legend<-ggplot(annualOutput)+
#    geom_line(aes(x=year, y=PSDQuality, color=as.factor(WBIC)), linewidth=1.5)+
#    scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
#    xlab("Year of simulation")+
#    ylab("PSD (Quality size)")+
#    labs(color="WBIC")+
#    theme_bw()
#  
#  legend<-get_legend(legend)
#  
#  plot_grid(meanSize, maxSize, psd, legend)
#  ggsave(paste0(base.directory, "/figures", "/size.structure.png"), height=8, width=12)
# 
