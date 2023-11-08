# calculate fishing mortality by age class

fishing.mortality<-function(y, fishery){
  # use change in fish population over 1 year for F
  fishPop<-fishery[["fishPop"]]
  harvestAge<-fishery[["harvestAge"]]
  FmortAge<-fishery[["FmortAge"]]
  # because fishPop starts with year 0--fish population at the beginning of the year
  thisYearStart<-fishPop[,y]
  
  yearsHarvest<-harvestAge[,y]
  
  FmortAge[,y]<-yearsHarvest/thisYearStart
  # for age classes where population is 0, will get NaN
  FmortAge[,y]<-ifelse(is.nan(FmortAge[,y]), 0, FmortAge[,y])
  
  fishery[["FmortAge"]]<-FmortAge
  return(fishery)
  
 # nope, need a harvest at age object
}