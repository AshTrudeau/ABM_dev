# start output table
initialize.lake.status<-function(parameters, lakeCharacteristics, fishery){
  
  nLakes<-parameters[["nLakes"]]
  nDays<-parameters[["nDays"]]
  nYears<-parameters[["nYears"]]
  N0<-parameters[["N0"]]
  
  WBIC<-lakeCharacteristics$WBIC
  
  fishPops<-fishery[["fishPops"]]
  
  # for each lake (matrix in fishPops list), add up the total number of fish, and then
  # put those values in the matching (WBIC) row for day and year 0
  
  startingFish<-lapply(fishPops, function(x) x[,1])
  startingNFish<-lapply(fishPops, sum)
  startingNFish<-unlist(lapply(startingNFish, round))
  
  day<-rep(0, nLakes)
  year<-rep(0, nLakes)
  fishN<-rep(NA, nLakes)
  harvestedN<-rep(0, nLakes)
  nAnglers<-rep(0, nLakes)
  
  lakeStatus0<-cbind.data.frame(WBIC, day, year, nAnglers, fishN, 
                                harvestedN
                                )
  lakeStatus0$fishN<-unname(startingNFish)
  

  WBIC<-rep(WBIC, nDays*nYears)
  day<-rep(rep(seq(1:nDays), each=nLakes), nYears)
  year<-rep(seq(1:nYears), each=nLakes*nDays)
  
  n<-nDays*nLakes*nYears
  
  fishN<-rep(NA, n)
  harvestedN<-rep(NA, n)
  nAnglers<-rep(NA, n)
  
  lakeStatusBlank<-cbind.data.frame(WBIC, day, year, nAnglers, fishN, 
                                    harvestedN 
                                    )
  
  lakeStatus<-rbind.data.frame(lakeStatus0, lakeStatusBlank)
  
  fishery[["lakeStatus"]]<-lakeStatus
  
  return(fishery)
}