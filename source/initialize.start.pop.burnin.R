initialize.start.pop.burnin<-function(parameters){
  nAges<-parameters[["nAges"]]
  N0<-parameters[["N0"]]
  nBurnIn<-parameters[["nBurnIn"]]
  nLakes<-parameters[["nLakes"]]
  
  # make matrix that includes age 0
  startPop<-matrix(data=0, nrow=nAges+1, ncol=nYears)
  startPop[1,1]<-N0
  
  startPops<-lapply(seq_len(nLakes), function(x) startPop)
  
  names(startPops)<-selectLakes$WBIC
  
  
  return(startPops)
}