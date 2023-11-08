initialize.FmortAge<-function(parameters){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  
  FmortAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  return(FmortAge)
}