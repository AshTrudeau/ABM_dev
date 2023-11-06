initialize.NmortAge<-function(parameters){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  
  NmortAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
}