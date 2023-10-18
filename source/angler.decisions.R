# lake.decisions() Anglers choose which lake to fish in iteration t
# for now, they just choose the closest lake. In future drafts, other variables
# will influence choices

angler.decisions<-function(fishery, t, y){
  lakeDistance<-fishery[["lakeDistance"]]
  anglerCharacteristics<-fishery[["anglerCharacteristics"]]
  anglerDecisions<-fishery[["anglerDecisions"]]
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  lakeStatus<-fishery[["lakeStatus"]]
  
  nDays<-parameters[["nDays"]]
  
  # remove old outputs. Results of previous loops go into lakeStatus
  anglerDecisions<-anglerDecisions[,c("anglerID","lakeID","catch","harvest")]
  anglerDecisions$lakeID<-rep(NA)
  anglerDecisions$catch<-rep(NA)
  anglerDecisions$harvest<-rep(NA)
  
  nAnglers<-parameters[["nAnglers"]]
  
  #add current fish population to lakeDistance df
  
  # first day of the year is a little different; go back to last day of previous year
  if(t==1 & y==1){
    lakeStatusPrevious<-lakeStatus%>%
      filter(day==0 & year==0)
  }
  
  if(t==1 & y!=1){
    lakeStatusPrevious<-lakeStatus%>%
      filter(day==nDays & year==y-1)
    
  }
  if(t!=1){
  
  lakeStatusPrevious<-lakeStatus%>%
    filter(day==t-1 & year==y )
  }
  
  # fill in lakeDistance columns with relevant lake attributes that update daily and annually
  # At some point, this will contain expected catch rates and maximum size. That version will eventually
  # require both lake-specific and angler-specific expectations. For now it's only 
  # fish population numbers.
 
   lakeDistancePop<-lakeDistance%>%
    dplyr::left_join(lakeStatusPrevious[,c("lakeID","fishN")], by="lakeID")
  

for(i in 1:nAnglers){
  
  indiv<-lakeDistancePop[lakeDistancePop$anglerID==i & lakeDistancePop$fishN>0,]
  # decision rule
  anglerDecisions[i,"lakeID"]<-indiv[which.min(indiv$distance),]$lakeID
}
  fishery[["anglerDecisions"]]<-anglerDecisions
  return(fishery)

}