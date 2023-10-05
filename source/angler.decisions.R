# lake.decisions() Anglers choose which lake to fish in iteration t
# for now, they just choose the closest lake. In future drafts, other variables
# will influence choices

angler.decisions<-function(fishery){
  lakeDistance<-fishery[["lakeDistance"]]
  anglerCharacteristics<-fishery[["anglerCharacteristics"]]
  anglerDecisions<-fishery[["anglerDecisions"]]
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # remove old outputs
  anglerDecisions<-anglerDecisions[,c(1:4)]
  anglerDecisions$lakeID<-rep(NA)
  anglerDecisions$catch<-rep(NA)
  anglerDecisions$harvest<-rep(NA)
  
  nAnglers<-parameters[["nAnglers"]]
  
  #add current fish population to lakeDistance df
  
  lakeDistancePop<-lakeDistance%>%
    dplyr::left_join(lakeCharacteristics[,c("lakeID","fishPop")], by="lakeID")
  

for(i in 1:nAnglers){
  
  indiv<-lakeDistancePop[lakeDistancePop$anglerID==i & lakeDistancePop$fishPop>0,]
  # decision rule
  anglerDecisions[i,"lakeID"]<-indiv[which.min(indiv$distance),]$lakeID
}
  fishery[["anglerDecisions"]]<-anglerDecisions
  return(fishery)

}