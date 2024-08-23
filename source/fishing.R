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
  nAges<-parameters[["nAges"]]
  
  # pull fish population matrix for this year. (y)

  fishPopYear<-lapply(fishPops, function(x) x[,y])
  


  # I've replaced the loop with a (hopefully faster) vectorized version
  
# pluck_multiple() solution from here https://stackoverflow.com/questions/46983716/does-a-multi-value-purrrpluck-exist 
  # I'm not totally sure how this function works, but it plucked multiple items from the fishPopYear list corresponding to 
  # WBICs chosen
  
  pluck_multiple <- function(x, ...) {
    `[`(x, ...)
  }

  # this is a list where every element is the fishPopYear entry for the lake chosen by that angler.
  #in otherwords, this is a list of 1000 vectors, one for each angler, with the current fish populations 
  # (at each age) that the anglers selected 

    fishPopYearChoices<-pluck_multiple(fishPopYear, anglerDecisions$WBIC)
  
  # now applying catch equation where each list entry is 1 angler at 1 lake. 
  q.select<-q*selectivity
  # raise fish populations to the linearity term in teh catch equation C=EqN^beta
  fishPop.beta<-lapply(fishPopYearChoices, `^`, beta)
  # Multiply by q to get catch in 1 hour
  catchAgeLake.1hr<-lapply(fishPop.beta, `*`, q.select)
  # multiply by 4 for catch in 4 hours
  catchAgeLake.4hr<-lapply(catchAgeLake.1hr, `*`, 4)
  # round it to an integer. Maybe this can be replaced with a draw from a negative binomial
  # distribution. The paper about this is in prep, and the estimated parameters could be useful here
  # (goal was to partition variance in catch between population density, angler skill, daily conditions, and blind luck)
  # currently this catch is totally deterministic

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
    mutate(age=rep(0:nAges, nAnglers))%>%
    rename("wbic"=ind,
           "catch"=values)%>%
    group_by(wbic, age)%>%
    summarize(catch=sum(catch))%>%
      ungroup()%>%
    select(-age)
  
  # the list is incomplete because it only has lakes that were visited by anglers (in anglerDecision matrix)
  splitCatchAgeLake<-split(stackCatchAgeLake, stackCatchAgeLake$wbic)
  splitCatchAgeLake<-lapply(splitCatchAgeLake, subset, select=-wbic)
  # this turns total catch back into a vector
  splitCatchAgeLake<-lapply(splitCatchAgeLake, deframe)
  
  # for the rest of the lakes that were unvisited, I'm making blank list elements to append to splitCatchAgeLake
  missingLakes<-setdiff(names(fishPopYear), names(splitCatchAgeLake))
  missingLakesList<-vector("list", length(missingLakes))
  
  for(i in 1:length(missingLakes)){
    if(length(missingLakes)==0){
      break
    }
        missingLakesList[[i]]<-rep(0, nAges+1)
      }

  names(missingLakesList)<-missingLakes
  
  catchAgeLakeAppend<-append(splitCatchAgeLake, missingLakesList)
  # now reorder to match fishPopYear
  
  harvestAgeToday<-catchAgeLakeAppend[names(fishPopYear)]
   #  now I can subtract harvestAgeYear from fishPopYear to get new fish populations
  
  newFishPopYear<-mapply(`-`, fishPopYear, harvestAgeToday, SIMPLIFY=F)
  
  # also add today's harvestAgeYear to yesterdays' harvest
  
  harvestAgeYesterday=lapply(harvestAge, function(x) x[,y])
  
  harvestAgeCumulative = mapply(`+`, harvestAgeYesterday, harvestAgeToday, SIMPLIFY=F)
  
  for(i in 1:length(fishPops)){
    harvestAge[[i]][,y]=harvestAgeCumulative[[i]]
  }

  for(i in 1:length(fishPops)){
  fishPops[[i]][,y]<-newFishPopYear[[i]]
  }


  
  # now update lakeStatus
  # I don't have an object to track total individual catch; I may need to add one later if 
  # individual catch rates (separate from harvest) are important for angler decisions.
  
  totalHarvest<-sapply(harvestAgeToday, sum)
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
