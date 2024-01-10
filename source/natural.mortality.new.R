natural.mortality<-function(y, parameters, fishery){
  
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
 
  fishPops<-fishery[["fishPops"]]
  
  lakeStatus<-fishery[["lakeStatus"]]
  
  # other option: constant M based on lake-specific growth params
  
  #M = 4.118*(K^0.73)*(linf^-0.33) (Then et al 2015)
}