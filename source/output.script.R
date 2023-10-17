output.script<-function(fishery, t, y, output, parameters){
  
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  anglerDecisions<-fishery[["anglerDecisions"]]
  
  lakeStatus<-output[["lakeStatus"]]
  
  nLakes<-parameters[["nLakes"]]
  
  
  lakeID<-lakeCharacteristics$lakeID
  harvestedN<-lakeCharacteristics$harvestedN
  harvestedB<-lakeCharacteristics$harvestedB
  
  fishN<-lakeCharacteristics$fishN
  fishB<-lakeCharacteristics$fishB
  # replace 1 with t and y
  day<-rep(t, nLakes)
  year<-rep(y, nLakes)
  
  nAnglers.sum<-anglerDecisions%>%
    dplyr::group_by(lakeID)%>%
    dplyr::summarize(nAnglers=n())%>%
    ungroup()
  
  # the order of columns matters!
  update<-cbind.data.frame(lakeID, day, year, fishN, fishB, harvestedN, harvestedB)%>%
    dplyr::left_join(nAnglers.sum, by=c("lakeID"))%>%
    # fill in 0 values
    dplyr::mutate(nAnglers=ifelse(is.na(nAnglers), 0, nAnglers))%>%
    dplyr::select(lakeID, day, year, nAnglers, fishN, fishB, harvestedN, harvestedB)
  
  #place update into lakeStatus where t = day and y = year
  # replace 1 with t and y
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]<-update
  
  # drop harvestedN and harvestedB from lakeCharacteristics for next loop
  fishery[["lakeCharacteristics"]]<-lakeCharacteristics[,c("lakeID","lakeClasses","fishN0","fishN","c","rho","weightVulnerable","meanWeight","fishB0","fishB")]
  
  output[["lakeStatus"]]<-lakeStatus
  return(output)
  }