# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, parameters, t, y){
  
  # this df has the fish populations as of the previous timestep
  lakeStatus<-fishery[["lakeStatus"]]

  # number of fish of each age class as of the previous timestep
  fishPops<-fishery[["fishPops"]]
  

  # harvest of each age class
  harvestAge<-fishery[["harvestAge"]]
  # age specific selectivity
  selectivity<-fishery[["selectivity"]]

  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  anglerDecisions$catch<-rep(NA)
  anglerDecisions$harvest<-rep(NA)
  

  beta<-parameters[["beta"]]
  q<-parameters[["q"]]
  nAnglers<-parameters[["nAnglers"]]
  
  
  # pull fish population matrix for this year. Note that years start on year 0. "this year's" column will then be y+1.
  
  # need to set conditions for first day of first year. Find total fish population (sum N) for each lake
  # if it's the very first timestep, the fish population is N0 for each lake
  
  # come back and vectorize this when I'm less time constrained
  
  if(t==1 & y==1){
    # if it's the first day of the first year, take the 'year zero' fish population
    fishPopYear<-lapply(fishPops, function(x) x[,y])
      
  } else{
    # take out the appropriate year's column from the fish population matrices
    fishPopYear<-lapply(fishPops, function(x) x[,y+1])

  }
  
  # pull harvestAge vector for this year
  harvestAgeYear<-lapply(harvestAge, function(x) x[,y])


  # function loops through anglers to calculate their catch (age specific)
  # I would like to eventually replace these for-loops with something 
  # less stupid.

  for(n in 1:nAnglers){
    # pull angler's chosen lake
    WBIC<-anglerDecisions[n,]$WBIC
    # pull population vector for that lake
    popAgeLake<-fishPopYear[[WBIC]]
    harvestAgeLake<-harvestAgeYear[[WBIC]]
    # calculate age-specific catch
    catchAgeLake<-round((q*selectivity*popAgeLake^beta)*4)
    # this is a problem
    # update anglerDecisions df with aggregate catch
    anglerDecisions[n,]$catch<-sum(catchAgeLake)
    # for now harvest is the same as catch
    anglerDecisions[n,]$harvest<-sum(catchAgeLake)
    
    # remove catch from the population
    popAgeLake<-popAgeLake-catchAgeLake
    harvestAgeLake<-harvestAgeLake+catchAgeLake
    
    # update the fishPopYear and harvestAge matrix
    
    fishPopYear[[WBIC]]<-popAgeLake
    harvestAgeYear[[WBIC]]<-harvestAgeLake

  }
  
  # put harvestAgeYear and fishPopYear back into harvestAge and fishPops. (this 'yearly' column updates daily as 
  # catch and harvest continue)


  for(i in 1:length(fishPops)){
  fishPops[[i]][,y+1]<-fishPopYear[[i]]
  }
  
  # do the same for harvestAge
  
  for(i in 1:length(harvestAge)){
    harvestAge[[i]][,y]<-harvestAgeYear[[i]]
  }
  
  
  # now update lakeStatus
  # I don't have an object to track total catch; I may need to add one later if 
  # individual catch rates (separate from harvest) are important for angler decisions.
  
  totalHarvest<-sapply(harvestAgeYear, sum)
  totalCatch<-totalHarvest
  
  totalPop<-sapply(fishPopYear, sum)
  
  totalEffort<-anglerDecisions%>%
    # arranges output in correct order (same order as lakeCharacteristics)
    mutate(WBIC=factor(WBIC, levels=lakeCharacteristics$WBIC))%>%
    group_by(WBIC, .drop=F)%>%
    summarize(nAnglers=n())%>%
    ungroup()
  
  # update lakeStatus
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]$fishN<-totalPop

  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]$harvestedN<-totalHarvest
  
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]$nAnglers<-totalEffort$nAnglers

  # note: daily catch/harvest is now tracked in lake status, but this is not age or size specific. If I want to track,
  # fish sizes, I'll need to add another object to the list. I'll also need to add a biomass calculation later
  
#  note that total harvest and total pop is cumulative through the year, but fishing effort is not. 
  fishery[["lakeStatus"]]<-lakeStatus
  fishery[["harvestAge"]]<-harvestAge
  fishery[["fishPops"]]<-fishPops
  
  return(fishery)
  

}
