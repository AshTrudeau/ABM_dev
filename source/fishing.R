# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, parameters, t, y){
  
  # this df has the catch coefficients
  #lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # and this df has the fish populations as of the previous timestep
  lakeStatus<-fishery[["lakeStatus"]]
  
  # number of fish of each age class
  fishPop<-fishery[["fishPop"]]
  # harvest of each age class
  harvestAge<-fishery[["harvestAge"]]
  # age specific selectivity
  selectivity<-fishery[["selectivity"]]
  

  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  
  #nAnglers<-parameters[["nAnglers"]]
  #nLakes<-parameters[["nLakes"]]
  
  beta<-parameters[["beta"]]
  q<-parameters[["q"]]
  
  # pull fish population matrix for this year. Note that years start on year 0. "this year's" column will then be y+1.
  
  # need to set conditions for first day of first year
  
  if(t==1 & y==1){
    fishPopYear<-fishPop[,1]
      
  } else{
    fishPopYear<-fishPop[,y+1]
  }

  startingPop<-sum(fishPopYear)
  
  # at some point, I'll need to do this for all lakes at once. will probably need to use purrr
  catch<-round((q*selectivity*fishPopYear^beta)*4)
  
  # for now the same
  harvest<-catch
  
  harvestAge[,y]<-harvestAge[,y]+harvest
  
  
  totalCatch<-sum(catch)
  totalHarvest<-sum(harvest)
  
  
  
  # update lakeStatus
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]$fishN<-startingPop-totalHarvest
  # update nAnglers when I add multiple lakes. For now it will stay NA
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]$harvestedN<-totalHarvest


  # update fishPop
  fishPop[,y+1]<-fishPopYear-harvest
  
  # note: daily catch/harvest is now tracked in lake status, but this is not age or size specific. If I want to track,
  # fish sizes, I'll need to add another object to the list. I'll also need to add a biomass calculation later
  

  fishery[["lakeStatus"]]<-lakeStatus
  fishery[["harvestAge"]]<-harvestAge
  fishery[["fishPop"]]<-fishPop
  
  return(fishery)
  

}