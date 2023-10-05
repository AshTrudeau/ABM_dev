# start output table
initialize.output.lakes<-function(parameters, lakeCharacteristics){
  nLakes<-parameters[["nLakes"]]
  
  lakeID<-lakeCharacteristics$lakeID
  day<-rep(0, nLakes)
  # add year 0 once I'm adding  year loop
  fishPop<-lakeCharacteristics$fishPop0
  nHarvested<-rep(0, nLakes)
  nAnglers<-rep(0, nLakes)
  
  lakeStatus<-cbind.data.frame(lakeID, day, fishPop, nHarvested, nAnglers)
  return(lakeStatus)
}