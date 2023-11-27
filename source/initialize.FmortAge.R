initialize.FmortAge<-function(parameters){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  nLakes<-parameters[["nLakes"]]
  
  FmortAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
  
  FmortAge<-lapply(seq_len(nLakes), function(x) FmortAge)
  
  names(FmortAge)<-selectLakes$WBIC
  
  return(FmortAge)
}