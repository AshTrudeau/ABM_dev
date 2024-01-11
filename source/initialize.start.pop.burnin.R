initialize.start.pop.burnin<-function(parameters){
  nAges<-parameters[["nAges"]]
  N0<-parameters[["N0"]]
  nBurnIn<-parameters[["nBurnIn"]]
  nLakes<-parameters[["nLakes"]]
  
  N0Lakes<-round(N0*lakeCharacteristics$areaHa)
  names(N0Lakes)<-lakeCharacteristics$WBIC
  N0LakesList<-as.list(N0Lakes)
  # year zero starts with 1000 age 1 fish
  startPop<-matrix(data=0, nrow=nAges+1, ncol=nBurnIn, dimnames=list(c(0:nAges), c(1:nBurnIn)))
  
  startPops<-lapply(seq_len(nLakes), function(x) startPop)
  
  startPops<-lapply(seq_along(startPops), function(x){
    lake_matrix<-startPops[[x]]
    lakeN0_name<-names(N0LakesList)[x]
    lake_vector<-c(N0LakesList[[lakeN0_name]], rep(0, nAges))
    lake_matrix[,1]<-lake_vector
    return(lake_matrix)
  })
  
  names(startPops)<-selectLakes$WBIC
  
  return(startPops)
}