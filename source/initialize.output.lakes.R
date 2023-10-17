# start output table
initialize.output.lakes<-function(parameters, lakeCharacteristics){
  
  nLakes<-parameters[["nLakes"]]
  nDays<-parameters[["nDays"]]
  nYears<-parameters[["nYears"]]
  
  lakeID<-lakeCharacteristics$lakeID
  
  day<-rep(0, nLakes)
  year<-rep(0, nLakes)
  fishN<-lakeCharacteristics$fishN0
  fishB<-fishN*lakeCharacteristics$meanWeight
  harvestedN<-rep(0, nLakes)
  harvestedB<-rep(0, nLakes)
  nAnglers<-rep(0, nLakes)
  
  # let's make the assumption for now that biomass = N * mean weight
  # plan to replace this simplification with an age and size structured model
  lakeStatus0<-cbind.data.frame(lakeID, day, year, nAnglers, fishN, fishB, harvestedN,
                                harvestedB)
  
  # replicate this for 5 years, each with 100 days, each with 10 lakes
  # adding 10 days for 'zero' starting day in year 1
  
  lakeID<-rep(lakeID, nDays*nYears)
  day<-rep(rep(seq(1:nDays), each=nLakes), nYears)
  year<-rep(seq(1:nYears), each=nLakes*nDays)
  
  n<-nDays*nLakes*nYears
  
  fishN<-rep(NA, n)
  fishB<-rep(NA, n)
  harvestedN<-rep(NA, n)
  harvestedB<-rep(NA, n)
  nAnglers<-rep(NA, n)
  
  lakeStatusBlank<-cbind.data.frame(lakeID, day, year, nAnglers, fishN, fishB, harvestedN, harvestedB)
  
  lakeStatus<-rbind.data.frame(lakeStatus0, lakeStatusBlank)
  return(lakeStatus)
}