# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, parameters){
  # this df has the catch coefficients
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  nAnglers<-parameters[["nAnglers"]]
  
  anglerDecisions<-anglerDecisions%>%
    dplyr::left_join(lakeCharacteristics, by="lakeID")%>%
    dplyr::mutate(catch=rpois(nAnglers, catchParam),
           harvest=catch)
  
  lakeHarvest<-anglerDecisions%>%
    dplyr::group_by(lakeID)%>%
    dplyr::summarize(nHarvested=sum(harvest))
  
  lakeCharacteristics<-lakeCharacteristics%>%
    left_join(lakeHarvest, by="lakeID")%>%
    # not every lake is visited each loop; fil in NA values with 0
    mutate(nHarvested=ifelse(is.na(nHarvested), 0, nHarvested),
          fishPop=fishPop-nHarvested)
  
  fishery[["anglerDecisions"]]<-anglerDecisions
  fishery[["lakeCharacteristics"]]<-lakeCharacteristics
  return(fishery)
  

}