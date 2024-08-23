# This script is an example template for running the ABM from functions in "source"
# This model simulates age-structured walleye populations from 13 trend lakes in Wisconsin. 
# I had planned to eventually expand this to also include largemouth bass and bluegill.
#=======================================================================
# directory stuff
rm(list=ls())
setwd("C:/Users/ashle/Dropbox/bluegill management postdoc/lakeSelectTool/ABM_dev/")
wd<-getwd()
base.directory<-wd
# Output gets written out to "output" folder
outdir<-paste0(base.directory, "/output/")
# load all functions, including a function that sets all model parameters
source(paste0(base.directory, "/source/function.sourcer.R"))
# show all parameters and their values
data.frame(unlist(parameters))

#----------------------------------------------------------------------
# need access to use this, but I only use it for visualizations at the end
library(RColorBrewer)
set.seed(38)


# This version of the simulation is starting to incorporate lake-specific (or lake class-specific) VBGF growth parameters. 
# The lake-specific parameters came from Paul Frater's Heirarchical Age Length Keys (https://doi.org/10.1002/fsh.11019)
# I only used the lake-specific estimates available in the RData below. The rest of the growth parameters come from 
# flexible lake classifications by Andrew Rypel et al 2019 ( https://doi.org/10.1002/fsh.10228)


load(paste0(base.directory, "/data/wi_halk_aged_growth_params.RData"))


# pulling out lake- and year-specific growth parameters with reasonable estimates, which leaves me with 23 lakes. 
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

# these are those lake class standard/mean growht params
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

# and these are all of the lakes classified in Rypel et al 2019.
allLakes<-read_csv(paste0(base.directory, "/", "data/", "all.lake.classes.wi.csv"))%>%
  rename("lakeClass"=`Final Lake Class`)%>%
  # filter(County=="Vilas")%>%
  filter(grepl("Complex", lakeClass) & !grepl("Riverine", lakeClass))%>%
  mutate(lakeClass=str_replace_all(lakeClass, " - ", " "), 
         WBIC=as.character(WBIC))%>%
  mutate(lakeSpecificGrowth=ifelse(WBIC%in%vbgf_lakeSpecific$WBIC, 1, 0))


# For some of these runs I drew a sample of lakes according to their proportional representation in WI
# those proportions are in this csv
#drawProp<-read_csv(paste0(base.directory, "/data/complex.lake.proportion.csv"))

# select.lakes() is a function in the "source" folder to draw a stratified random sample of lakes to simulate
#selectLakes<-select.lakes(parameters, allLakes, drawProp)

# for this run I wanted to use the trend lakes that are repeatedly sampled by the WI DNR (and therefore have
# lots of data to compare with the simulation outputs)

trendLakes<-as.character(read_rds(paste0(base.directory, "/source/trend.wbics.Rds")))

selectLakes<-allLakes%>%
  filter(WBIC%in%trendLakes)
# no lake specific growth available (which is weird, may want to check with Paul), so using lake classes

# this is another function in "source" that creates a table of lakes used by the simulation. The table 
# includes location (latlong) class, and growth parameters. It then adds on the recruitment parameters
# (alpha and beta) specified in "parameters.R". For now these are just regional parameters estimated by 
# Tsehaye et al 2016 (https://doi.org/10.1139/cjfas-2015-0191). 

lakeCharacteristics<-lake.characteristics(parameters, selectLakes, vbgf_lakeClass)

# this places anglers randomly on a grid. Could eventually add other characteristics besides location or
# assign anglers to locations corresponding to population centers. The number of anglers and the bounds
# of angler placement are defined in parameters.R
anglerCharacteristics<-angler.characteristics(parameters)

# spot check  on locations of anglers and lakes
ggplot()+
   geom_point(data=anglerCharacteristics, aes(x=anglerLong, y=anglerLat))+
   geom_point(data=lakeCharacteristics, aes(x=lakeLong, lakeLat), color="blue", size=4)


# this finds straight-line distances between anglers and lakes. Eventually this could use Gmaps API to find actual
# travel times, but that will cost money. 
# the function puts out a long tibble with the distance between every angler and every lake. This might be a dumb
# and slow way to do this, idk
lakeDistance<-lake.distance(lakeCharacteristics, anglerCharacteristics)

# this makes the blank table where angler site choices will be filled in during every iteration
anglerDecisions<-create.blank.angler.decisions(parameters)


# Setting up fish population matrix as a nested list with 1 matrix per lake
# number of age 0  fish in first step of burn in is N0*lake area
fishPops<-initialize.fish.pop.burnin(parameters, lakeCharacteristics)


# This makesa copy of fishPops to keep track of starting population of each year. in contrast, fishPops is 
# updated each day of each year
startPops<-initialize.start.pop.burnin(parameters)

# This makes a vector for age-specific selectivity (of fishing). ageVulnerable is set in params, adn this is 
# only set up for full vulnerability (1) or no vulnerability (0)
selectivity<-initialize.selectivity(parameters)

# make a harvest at age object (need this to estimate age specific F later). 
# this object keeps track of how many fish of each age are harvested
harvestAge<-initialize.harvestAge(parameters)

# FmortAge matrix will hold the age specific fishing mortality of each age class in each lake each year
FmortAge<-initialize.FmortAge(parameters)

# this will hold the natural mortality of each age class in each lake and year. Different versions of this 
# script are saved in "source" to initialize for constant natural mortality or Charnov natural mortality 
# defined by growth params
NmortAge<-initialize.NmortAge(parameters)

# this list of data frames holds length at age for each lake
fishSizes<-fish.size(parameters, lakeCharacteristics)

# Now putting all of those outputs into a master list that will hold important output from 
# each loop of the simulation

fishery<-list(anglerCharacteristics=anglerCharacteristics, 
              lakeDistance=lakeDistance, 
              lakeCharacteristics=lakeCharacteristics,
              anglerDecisions=anglerDecisions,
              # lakeStatus gets added in during the simulation; hold its place in fishery list
              lakeStatus=NA,
              fishPops=fishPops,
              fishSizes=fishSizes,
              startPops=startPops,
              harvestAge=harvestAge,
              selectivity=selectivity,
              FmortAge=FmortAge,
              NmortAge=NmortAge)

# Run "burn in" to equilibrium fish populations. 50 years worked well enough but is probably overkill
for(y in 1:nBurnIn){
  # These are some of the same functions used in the fishing simulation, burnin=TRUE skips F mort calculation
  fishery<-natural.mortality(y, fishery, parameters, burnin=TRUE)
  fishery<-ageing(y, fishery, parameters, burnin=TRUE)
  fishery<-recruitment(y, fishery, parameters, burnin=TRUE)
  fishery<-update.fishPops(y, fishery, parameters, burnin=TRUE)
  # for troubleshooting
  print(y)
}

# to see population matrices after nBurnIn years of no fishing mortality
#fishery$fishPops

# update startPops with equilibrium fish population 
# startPops is the *starting* population of the final year of burn-in. 
# (new) fishPops is ALSO the starting population of the final year of burn-in. 
# (reason: recruitment does not take place during final year of burn-in)

# the last year's starting population from burnin is now year 1 of the fishign simulation
fishery<-initialize.start.pop(parameters, fishery)

# fishPops is replaced (for now) with startPops. Harvest, natural mortality, and recruitment will
# occur in the fishPops nested list.
fishery<-initialize.fish.pop(parameters, lakeCharacteristics, fishery)

# Each iteration (day year), lakeStatus will be updated with the current fish populations
# to initialize, add up nFish in each lake for start of simulation (day 0 year 0)
fishery<-initialize.lake.status(parameters, lakeCharacteristics, fishery)

# dataframe  holding important annual output: changes in fish biomass and abundance, 
# metrics of fish size, total fishin geffort
annualOutput<-initialize.annual.output(parameters, fishery)


# Now the simulation finally starts. This is a nested loop; the outer loop is for annual changes in fish
# populations from natural mortality, reproduction, and growth. The inner loop is for daily changes in 
# fish populations from harvest. 

for(y in 1:parameters[["nYears"]]){
  # for troubleshooting
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

sim.annual<-annualOutput

effort<-ggplot(annualOutput)+
  geom_line(aes(x=year, y=annualEffort, color=WBIC), linewidth=1.5)+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  xlab("Year of simulation")+
  ylab("Fishing effort (angler visits per year)")+
  guides(color="none")+
  theme_bw()

harvest<-ggplot(annualOutput)+
  geom_line(aes(x=year, y=annualHarvestN, color=WBIC), linewidth=1.5)+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  xlab("Year of simulation")+
  ylab("Annual harvest (N)")+
  guides(color=guide_legend(title="WBIC"))+
  theme_bw()

fishPop<-ggplot(annualOutput)+
  geom_line(aes(x=year, y=fishNEnd, color=WBIC), linewidth=1.5)+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  xlab("Year of simulation")+
  ylab("End of year population (N)")+
  guides(color="none")+
  theme_bw()

effort<-ggplot(annualOutput)+
  geom_line(aes(x=year, y=annualEffort, color=WBIC), linewidth=1.5)+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  xlab("Year of simulation")+
  ylab("Fishing effort (angler visits per year)")+
  guides(color="none")+
  theme_bw()

harvest<-ggplot(annualOutput)+
  geom_line(aes(x=year, y=annualHarvestN, color=WBIC), linewidth=1.5)+
   scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  xlab("Year of simulation")+
  ylab("Annual harvest (N)")+
  guides(color=guide_legend(title="WBIC"))+
  theme_bw()

fishPop<-ggplot(annualOutput)+
  geom_line(aes(x=year, y=fishNEnd, color=WBIC), linewidth=1.5)+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  xlab("Year of simulation")+
  ylab("End of year population (N)")+
  guides(color="none")+
  theme_bw()

lakeOutput<-annualOutput%>%
  group_by(WBIC)%>%
  summarize(effort=sum(annualEffort))%>%
  ungroup()%>%
  mutate(WBIC=as.character(WBIC))%>%
  left_join(lakeCharacteristics, by="WBIC")%>%
  mutate(WBIC=factor(WBIC, levels=levels(annualOutput$WBIC)))


hotspots<-ggplot()+
  geom_point(data=lakeOutput, aes(x=lakeLong, y=lakeLat, color=WBIC, size=effort))+
  geom_point(data=anglerCharacteristics, aes(x=anglerLong, y=anglerLat), shape=4)+
  xlab("X coord")+
  ylab("Y coord")+
  theme_bw()



hotspots<-ggplot()+
  geom_point(data=lakeOutput, aes(x=lakeLong, y=lakeLat, color=WBIC, size=effort))+
  geom_point(data=anglerCharacteristics, aes(x=anglerLong, y=anglerLat), shape=4)+
  #scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  guides(color="none",
         size=guide_legend(title="Fishing\neffort"))+
  xlab("X coord")+
  ylab("Y coord")+
  theme_bw()


 meanSize<-ggplot(annualOutput)+
   geom_line(aes(x=year, y=meanSize, color=as.factor(WBIC)), linewidth=1.5)+
   scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
   xlab("Year of simulation")+
   ylab("Mean walleye length (cm)")+
   guides(color="none")+
   theme_bw()
 
 maxSize<-ggplot(annualOutput)+
   geom_line(aes(x=year, y=maxSize, color=as.factor(WBIC)), linewidth=1.5)+
   scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
   xlab("Year of simulation")+
   ylab("Max walleye length (cm)")+
   guides(color="none")+
   theme_bw()
 
 psd<-ggplot(annualOutput)+
   geom_line(aes(x=year, y=PSDQuality, color=as.factor(WBIC)), linewidth=1.5)+
   scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
   xlab("Year of simulation")+
   ylab("PSD (Quality size)")+
   guides(color="none")+
   theme_bw()
