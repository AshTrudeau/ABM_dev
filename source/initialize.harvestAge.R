# tracking the number of fish of each age class harvested each year

initialize.harvestAge<-function(parameters){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  
  harvestAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  return(harvestAge)
  
  
}