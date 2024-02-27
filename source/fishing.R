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
  nLakes<-parameters[["nLakes"]]
  
  # pull fish population matrix for this year. (y)

  fishPopYear<-lapply(fishPops, function(x) x[,y])
  
  # pull harvestAge vector for this year (it's blank)
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
  # fishPopYearChoices is length1000
  fishPopYearChoices<-pluck_multiple(fishPopYear, anglerDecisions$WBIC)
  
  # now applying catch equation where each list entry is 1 angler at 1 lake. 
  q.select<-q*selectivity
  fishPop.beta<-lapply(fishPopYearChoices, `^`, beta)
  catchAgeLake.1hr<-lapply(fishPop.beta, `*`, q.select)
  catchAgeLake.4hr<-lapply(catchAgeLake.1hr, `*`, 4)
  catchAgeLake.round<-lapply(catchAgeLake.4hr, round)
  
  #  add up total number caught and harvested, add to anglerDecisions matrix
  catchCol<-lapply(catchAgeLake.round, sum)
  catchCol.matrix<-unlist(catchCol)
  anglerDecisions$catch<-catchCol.matrix
  anglerDecisions$harvest<-catchCol.matrix
  

  # I want to subtract age-specific harvest from matching lakes in fishPopYear. 
  # This means that I need a list that looks like fishPopYear (in same order) with 
  # lakes that were harvested AND 0 harvest lakes.
  
  # this turns the catchAgeLake.round list (which has catch of each age class) into a dataframe.
  # This sums up all catch/harvest by lake and age, which can then be split back into a list
  stackCatchAgeLake<-stack(catchAgeLake.round)%>%
    mutate(age=rep(0:15, nAnglers))%>%
    rename("wbic"=ind,
           "catch"=values)%>%
    group_by(wbic, age)%>%
    summarize(catch=sum(catch))%>%
      ungroup()%>%
    select(-age)
  
  # the list is incomplete because it only has lakes that were visited by anglers (in anglerDecision matrix)
  splitCatchAgeLake<-split(stackCatchAgeLake, stackCatchAgeLake$wbic)
  splitCatchAgeLake<-lapply(splitCatchAgeLake, subset, select=-wbic)
  # this turns total catch back into a v ector
  splitCatchAgeLake<-lapply(splitCatchAgeLake, deframe)
  
  # for the rest of the lakes that were unvisited, I'm making blank list elements to append to splitCatchAgeLake
  missingLakes<-setdiff(names(fishPopYear), names(splitCatchAgeLake))
  missingLakesList<-vector("list", length(missingLakes))
  for(i in 1:length(missingLakes)){
    missingLakesList[[i]]<-rep(0, nAges+1)
  }
  names(missingLakesList)<-missingLakes
  
  catchAgeLakeAppend<-append(splitCatchAgeLake, missingLakesList)
  # now reorder to match fishPopYear
  
  harvestAgeYear<-catchAgeLakeAppend[names(fishPopYear)]
   #  now I can subtract harvestAgeYear from fishPopYear to get new fish populations
  
  # -------------------------------------
  # adding splitCatchAge to allLakesList to make harvestAgeYear

  # this is that aggregation step: 
  fishCaughtWBIC<-pluck_multiple(catchAgeLake.round, rep(filterCatch$WBIC, filterCatch$n))
  stack<-stack(fishCaughtWBIC)
  stack$age<-rep(0:15, nrow(stack)/16)
  catchAgeWBIC<-stack%>%
    rename("WBIC"=ind,
           "catch"=values)%>%
    group_by(WBIC, age)%>%
    mutate(catch=sum(catch))
  
  catchByAge<-split(catchAgeWBIC, catchAgeWBIC$WBIC)
  
  # to make harvestAgeYear, add aggregate age and lake specific catch to allLakesList
  
  
  
  # I also want to fill harvestAgeYear with age specific harvest from lakes
  
  # These are lakes where harvest took place
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
