# start output table
initialize.output.lakes<-function(parameters, lakeCharacteristics){
  
  nLakes<-parameters[["nLakes"]]
  nDays<-parameters[["nDays"]]
  nYears<-parameters[["nYears"]]
  
  lakeID<-lakeCharacteristics$lakeID
  
  day<-rep(0, nLakes)
  year<-rep(0, nLakes)
  fishPop<-lakeCharacteristics$fishPop0
  nHarvested<-rep(0, nLakes)
  nAnglers<-rep(0, nLakes)
  
  lakeStatus0<-cbind.data.frame(lakeID, day, year, fishPop, nHarvested, nAnglers)
  
  lakeID<-rep(lakeID, nDays)
  day<-rep(seq(1:nDays), each=nLakes)
  
  year<-rep(seq(1:nYears), each=nDays)
  day.rep<-rep(day, nYears)
  lakeID.rep<-rep(lakeID, nYears)
  
  lakeStatusBlank<-data.frame(lakeID=lakeID.rep, day=day.rep, year=year, fishPop=rep(NA), nHarvested=rep(NA), nAnglers=rep(NA))
  lakeStatus<-rbind.data.frame(lakeStatus0, lakeStatusBlank)
  return(lakeStatus)
}