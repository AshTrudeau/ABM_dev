initialize.fish.pop<-function(parameters, lakeCharacteristics){
  N0<-parameters[["N0"]]
  nAges<-parameters[["nAges"]]
  nYears<-parameters[["nYears"]]
  nLakes<-parameters[["nLakes"]]
  
  # year zero starts with 1000 age 1 fish
  fishPop<-matrix(data=0, nrow=nAges+1, ncol=1+nYears, dimnames=list(c(0:nAges), c(0:nYears)))
  
  # for now all lakes start with the same N0
  fishPop[1,1]<-N0
  
  fishPops<-lapply(seq_len(nLakes), function(x) fishPop)

  names(fishPops)<-selectLakes$WBIC
  
  return(fishPops)
}