# lake.decisions() Anglers choose which lake to fish in iteration t
# for now, they just choose the closest lake. In future drafts, other variables
# will influence choices

angler.decisions<-function(fishery){
  lakeDistance<-fishery[["lakeDistance"]]
  anglerCharacteristics<-fishery[["anglerCharacteristics"]]
  anglerDecisions<-fishery[["anglerDecisions"]]
  
  nAnglers<-parameters[["nAnglers"]]
  
  #anglerLocation$lakeChoice<-rep(NA)

for(i in 1:nAnglers){
  
  indiv<-lakeDistance[lakeDistance$anglerID==i,]
  # decision rule
  anglerDecisions[i,"lakeID"]<-indiv[which.min(indiv$distance),]$lakeID
}
  fishery[["anglerDecisions"]]<-anglerDecisions
  return(fishery)

}