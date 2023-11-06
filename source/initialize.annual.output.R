initialize.annual.output<-function(parameters, fishery){
  
  nYears=parameters[["nYears"]]
  nLakes=parameters[["nLakes"]]
  # taking out M because it's now age-specific
  #M=parameters[["M"]]
  
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # make dataframe to hold lakeID, year, mortality, and exploitation
  lakeID<-rep(lakeCharacteristics$lakeID, nYears)
  year<-rep(seq(1:nYears), each=nLakes)
  
  fishNStart<-rep(NA, nYears*nLakes)
  #fishBStart<-rep(NA, nYears*nLakes)
  
  annualHarvestN<-rep(NA, nYears*nLakes)
  #annualHarvestB<-rep(NA, nYears*nLakes)
  annualEffort<-rep(NA, nYears*nLakes)
  
  # currently filling in constant M
  #M<-rep(M, nYears*nLakes)
  exploitation<-rep(NA, nYears*nLakes)
  survival<-rep(NA, nYears*nLakes)
  recruitment<-rep(NA, nYears*nLakes)
  
  fishNEnd<-rep(NA, nYears*nLakes)
  #fishBEnd<-rep(NA, nYears*nLakes)
  

  annualOutput<-cbind.data.frame(lakeID, year, fishNStart, 
                                 #fishBStart, 
                                 annualHarvestN, 
                                 #annualHarvestB, 
                                 annualEffort, 
                                 #M, 
                                 exploitation, survival, recruitment,
                                 fishNEnd
                                 #, fishBEnd
                                 )

  return(annualOutput)
  
}