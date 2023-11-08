initialize.annual.output<-function(parameters, fishery){
  
  nYears=parameters[["nYears"]]
  nLakes=parameters[["nLakes"]]

  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # make dataframe to hold lakeID, year, mortality, and exploitation
  lakeID<-rep(lakeCharacteristics$lakeID, nYears)
  year<-rep(seq(1:nYears), each=nLakes)
  
  fishNStart<-rep(NA, nYears*nLakes)
  
  fishNEnd<-rep(NA, nYears*nLakes)

  annualHarvestN<-rep(NA, nYears*nLakes)

  annualEffort<-rep(NA, nYears*nLakes)
  

  annualOutput<-cbind.data.frame(lakeID, year, 
                                 fishNStart, fishNEnd,
                                 annualHarvestN, 
                                 annualEffort)

  return(annualOutput)
  
}