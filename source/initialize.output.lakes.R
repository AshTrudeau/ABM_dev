# start output table
initialize.output.lakes<-function(parameters, lakeCharacteristics){
  nLakes<-parameters[["nLakes"]]
  nDays<-parameters[["nDays"]]
  
  lakeID<-lakeCharacteristics$lakeID
  day<-rep(0, nLakes)
  # add year 0 once I'm adding  year loop
  fishPop<-lakeCharacteristics$fishPop0
  nHarvested<-rep(0, nLakes)
  nAnglers<-rep(0, nLakes)
  
  lakeStatus0<-cbind.data.frame(lakeID, day, fishPop, nHarvested, nAnglers)
  lakeStatusBlank<-data.frame(lakeID=rep(lakeID, nDays), day=rep(seq(1:nDays), each=nLakes), fishPop=rep(NA), nHarvested=rep(NA), nAnglers=rep(NA))
  lakeStatus<-rbind.data.frame(lakeStatus0, lakeStatusBlank)
  return(lakeStatus)
}