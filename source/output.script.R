output.script<-function(fishery, t, y, output, parameters){
  # update lake status
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  anglerDecisions<-fishery[["anglerDecisions"]]
    
  lakeStatus<-output[["lakeStatus"]]
  
  nLakes<-parameters[["nLakes"]]
  
  
  lakeID<-lakeCharacteristics$lakeID
  nHarvested<-lakeCharacteristics$nHarvested
  fishPop<-lakeCharacteristics$fishPop
  day<-rep(t, nLakes)
  year<-rep(y, nLakes)
  
  nAnglers.sum<-anglerDecisions%>%
    dplyr::group_by(lakeID)%>%
    dplyr::summarize(nAnglers=n())%>%
    ungroup()
  
  update<-cbind.data.frame(lakeID, day, year, fishPop, nHarvested)%>%
    dplyr::left_join(nAnglers.sum, by="lakeID")%>%
    # fill in 0 values
    dplyr::mutate(nAnglers=ifelse(is.na(nAnglers), 0, nAnglers))
  
  #place update into lakeStatus where t = day and y = year
  
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]<-update
  
  output[["lakeStatus"]]<-lakeStatus
  return(output)
  
  # update fishing effort
}