# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, parameters){
  # this df has the catch coefficients
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  nAnglers<-parameters[["nAnglers"]]
  
  anglerDecisions<-anglerDecisions%>%
    dplyr::left_join(lakeCharacteristics, by="lakeID")%>%
    mutate(catch=rpois(nAnglers, catchParam),
           harvest=catch)
  
  fishery[["anglerDecisions"]]<-anglerDecisions
  return(fishery)
  

}