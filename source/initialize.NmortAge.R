initialize.NmortAge<-function(parameters){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  nLakes<-parameters[["nLakes"]]
  
  NmortAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
  NmortAge<-lapply(seq_len(nLakes), function(x) NmortAge)
  
  names(NmortAge)<-selectLakes$WBIC
  
  return(NmortAge)
  
}