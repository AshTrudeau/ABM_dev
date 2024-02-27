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
  
  
  # pull fish population matrix for this year. (y)
  
  # come back and vectorize this when I'm less time constrained
  # vectorization in progress
  
  fishPopYear<-lapply(fishPops, function(x) x[,y])
  

    
  # pull harvestAge vector for this year
  harvestAgeYear<-lapply(harvestAge, function(x) x[,y])


  # function loops through anglers to calculate their catch (age specific)
  # I'm replacing the loop with a (hopefully faster) vectorized version
  
# solution from here https://stackoverflow.com/questions/46983716/does-a-multi-value-purrrpluck-exist 
  # I have no idea how this funciton works, but it plucked multiple items from the fishPopYear list corresponding to 
  # WBICs chosen
  
  pluck_multiple <- function(x, ...) {
    `[`(x, ...)
  }

  # this is a list where every element is the fishPopYear entry for the lake chosen by that angler.
  fishPopYearChoices<-pluck_multiple(fishPopYear, anglerDecisions$WBIC)
  
  q<-0.0015
  q.select<-q*selectivity
  fishPop.beta<-lapply(fishPopYearChoices, `^`, beta)
  catchAgeLake.1hr<-lapply(fishPop.beta, `*`, q.select)
  catchAgeLake.4hr<-lapply(catchAgeLake.1hr, `*`, 4)
  catchAgeLake.round<-lapply(catchAgeLake.4hr, round)
  
  catchCol<-lapply(catchAgeLake.round, sum)
  catchCol.matrix<-unlist(catchCol)
  anglerDecisions$catch<-catchCol.matrix
  anglerDecisions$harvest<-catchCol.matrix
  # no longer need aggregate catch across all age classes--keep age structure
  # for f iltering step
  filterCatch<-anglerDecisions%>%
    group_by(WBIC)%>%
    summarize(nHarvested=sum(harvest), 
              n=n())%>%
    ungroup()%>%
    filter(nHarvested!=0)
  
  # remove age-specific catch from the population
  
  #  first step, need to aggregate total catch by age for each lake chosen
  # simplest way may be to filter catchAgeLake.round to only lakes where fish were caught
 fishCaughtWBIC<-pluck_multiple(catchAgeLake.round, rep(filterCatch$WBIC, filterCatch$n))
  stack<-stack(fishCaughtWBIC)
  stack$age<-rep(0:15, nrow(stack)/16)
  catchAgeWBIC<-stack%>%
    rename("WBIC"=ind,
           "catch"=values)%>%
    group_by(WBIC, age)%>%
    mutate(catch=sum(catch))
  #  this has only lakes where >0 f ish were caught
  catchByAge<-split(catchAgeWBIC, catchAgeWBIC$WBIC)
  
  # now make full list of catch by age
  
  allLakesList<-vector("list", length(catchAgeLake.round))
  names(allLakesList)<-names(catchAgeLake.round)
  
  # and fill in catch at age
  

  # try unlisting, group_by, sum, then making it back into a list
  # this is where I left off 2/27
  
# this below didn't w ork because I need catch at age  
  # unlistCatchCol<-unlist(catchCol)
  # unlistCatchCol.df<-data.frame(WBIC=names(unlistCatchCol), catch=unname(unlistCatchCol))%>%
  #   group_by(WBIC)%>%
  #   summarize(catch=sum(catch))
  # 
  # aggCatchLake<-data.frame(WBIC=names(fishPopYear))%>%
  #   left_join(unlistCatchCol.df, by="WBIC")%>%
  #   mutate(catch=ifelse(is.na(catch), 0, catch))
  # 
  # catchColList<-split(aggCatchLake, aggCatchLake$WBIC)
  # aggCatchLake<-catchColList[names(fishPopYear)]
  # 
  # # now subtract harvest from fishPopYear
  # aggCatchLake2<-lapply(aggCatchLake, "[", "catch")
  # 
  # 
  # newFishPopYear<-mapply(`-` , fishPopYear, aggCatchLake2)
  
  # update the fishPopYear and harvestAge matrix
  
  for(n in 1:nAnglers){
    n<-1
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
  fishPops[[i]][,y]<-fishPopYear[[i]]
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
