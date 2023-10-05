# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, t, parameters){
  # this df has the catch coefficients
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  # if this is the first iteration (t=1 and, once year loop is added, y=1), make a new column: fishPop, and copy fishPop0 into it
  
  if(t==1){fishPop<-lakeCharacteristics$fishPop0} else
  {fishPop<-lakeCharacteristics$fishPop}
  
  
  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  nAnglers<-parameters[["nAnglers"]]
  
  anglerDecisions<-anglerDecisions%>%
    # joining lakeCharacteristics, but without fishPop0 (column 3)
    dplyr::left_join(lakeCharacteristics[,-3], by="lakeID")%>%
    dplyr::mutate(catch=rpois(nAnglers, catchParam),
           harvest=catch)
  
  lakeHarvest<-anglerDecisions%>%
    dplyr::group_by(lakeID)%>%
    dplyr::summarize(nHarvested=sum(harvest))%>%
    ungroup()
  
  lakeCharacteristics$fishPop<-fishPop
  
  lakeCharacteristics<-lakeCharacteristics%>%
    dplyr::left_join(lakeHarvest, by="lakeID")%>%
    # not every lake is visited each loop; fil in NA values with 0
    dplyr::mutate(nHarvested=ifelse(is.na(nHarvested), 0, nHarvested),
          fishPop=fishPop-nHarvested)
  
  fishery[["anglerDecisions"]]<-anglerDecisions
  fishery[["lakeCharacteristics"]]<-lakeCharacteristics
  return(fishery)
  

}