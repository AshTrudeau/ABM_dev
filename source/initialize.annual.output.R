initialize.annual.output<-function(parameters, lakeCharacteristics){
  
  nYears=parameters[["nYears"]]
  nLakes=parameters[["nLakes"]]
  M=parameters[["M"]]
  
  # make dataframe to hold lakeID, year, mortality, and exploitation
  lakeID<-rep(lakeCharacteristics$lakeID, nYears)
  year<-rep(seq(1:nYears), each=nLakes)
  fishPopStart<-rep(NA, nYears)
  annualHarvest<-rep(NA, nYears)
  annualEffort<-rep(NA, nYears)
  # currently filling in constant M
  M<-rep(M, nYears)
  exploitation<-rep(NA, nYears)
  survival<-rep(NA, nYears)
  
  annualOutput<-cbind.data.frame(lakeID, year, fishPopStart, annualHarvest, annualEffort, M, exploitation, survival)
  
  # in case I need to add other objects, make it a list
  annualOutput<-list(annualOutput=annualOutput)
  
  return(annualOutput)
  
}