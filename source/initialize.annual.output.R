initialize.annual.output<-function(parameters, fishery){
  

  nYears=parameters[["nYears"]]
  nLakes=parameters[["nLakes"]]

  #lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # make dataframe to hold lakeID, year, and lake-year summary statistics
  WBIC<-rep(lakeCharacteristics$WBIC, nYears)
  year<-rep(seq(1:nYears), each=nLakes)
  
  fishNStart<-rep(NA, nYears*nLakes)
  fishBStart<-rep(NA, nYears*nLakes)
  
  fishNEnd<-rep(NA, nYears*nLakes)
  fishBEnd<-rep(NA, nYears*nLakes)

  annualHarvestN<-rep(NA, nYears*nLakes)
  annualHarvestB<-rep(NA, nYears*nLakes)
  
  annualEffort<-rep(NA, nYears*nLakes)
  maxSize<-rep(NA, nYears*nLakes)
  meanSize<-rep(NA, nYears*nLakes)
  # proportional stock density of quality size (Gabelhouse 1984) gives 14.8-16.8 for walleye. I'll use 15
  PSDQuality<-rep(NA, nYears*nLakes)
  
  

  annualOutput<-cbind.data.frame(WBIC, year, 
                                 fishNStart, fishBStart,
                                 fishNEnd, fishBEnd, 
                                 annualHarvestN, annualHarvestB,
                                 annualEffort, maxSize, meanSize, PSDQuality)

  return(annualOutput)
  
}