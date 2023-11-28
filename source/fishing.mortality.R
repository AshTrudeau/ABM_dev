# calculate fishing mortality by age class

fishing.mortality<-function(y, fishery){
  # use change in fish population over 1 year for F
  fishPops<-fishery[["fishPops"]]
  startPops<-fishery[["startPops"]]
  harvestAge<-fishery[["harvestAge"]]
  FmortAge<-fishery[["FmortAge"]]
  
  yearsHarvest<-lapply(harvestAge, function(x) x[,y])
  yearsStartPops<-lapply(startPops, function(x) x[,y])
  #yearsFmortAge<-lapply(FmortAge, function(x) x[,y])
  
  divide<-function(x,y){
    x/y
  }
  
  yearsFmortAge<-mapply(divide, yearsHarvest, yearsStartPops)
  # replace NaN with zero (divided by zero because zero population)
  yearsFmortAge[is.nan(yearsFmortAge)]<-0
  # make back into a list
  yearsFmortAge<-as.list(as.data.frame(yearsFmortAge))
  
  # now replace that year's column with this year's FmortAge
  # 
  wbics<-names(FmortAge)
  
  FmortAge<-lapply(seq_along(FmortAge), function(x){
    lake_matrix<-FmortAge[[x]]
    lake_name<-names(yearsFmortAge)[x]
    lake_vector<-yearsFmortAge[[lake_name]]
    
    lake_matrix[,y]<-lake_vector
    return(lake_matrix)
    return(x)
  })
  names(FmortAge)<-wbics


  fishery[["FmortAge"]]<-FmortAge
  return(fishery)
  
}