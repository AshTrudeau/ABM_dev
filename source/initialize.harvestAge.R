# tracking the number of fish of each age class harvested each year

initialize.harvestAge<-function(parameters){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  nLakes<-parameters[["nLakes"]]
  
  harvestAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
  harvestAge<-lapply(seq_len(nLakes), function(x) harvestAge)
  
  names(harvestAge)<-selectLakes$WBIC
  
  
  return(harvestAge)
  
  
}