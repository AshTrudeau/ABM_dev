# calculate fishing mortality by age class

fishing.mortality<-function(y, fishery){
  # use change in fish population over 1 year for F
  fishPop<-fishery[["fishPop"]]
  startPop<-fishery[["startPop"]]
  harvestAge<-fishery[["harvestAge"]]
  FmortAge<-fishery[["FmortAge"]]

  yearsHarvest<-harvestAge[,y]
  
  FmortAge[,y]<-yearsHarvest/startPop
  
  FmortAge[,y]<-ifelse(is.nan(FmortAge[,y]), 0, FmortAge[,y])
  
  fishery[["FmortAge"]]<-FmortAge
  return(fishery)
  
 # nope, need a harvest at age object
}