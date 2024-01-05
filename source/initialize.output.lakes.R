# start output table
initialize.output.lakes<-function(parameters, lakeCharacteristics){
  
  nLakes<-parameters[["nLakes"]]
  nDays<-parameters[["nDays"]]
  nYears<-parameters[["nYears"]]
  N0<-parameters[["N0"]]
  
  WBIC<-lakeCharacteristics$WBIC
  
  day<-rep(0, nLakes)
  year<-rep(0, nLakes)
  fishN<-N0
  harvestedN<-rep(0, nLakes)
  nAnglers<-rep(0, nLakes)
  
  lakeStatus0<-cbind.data.frame(WBIC, day, year, nAnglers, fishN, 
                                harvestedN
                                )
  

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
  return(lakeStatus)
}