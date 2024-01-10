initialize.fish.pop<-function(parameters, lakeCharacteristics, fishery){
  # Before the first time step, startPops and fishPops look the same
  
  startPops<-fishery[["startPops"]]
  fishery[["fishPops"]]<-startPops
  
  return(fishery)
  }