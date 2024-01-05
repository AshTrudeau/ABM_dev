# lake.decisions() Anglers choose which lake to fish in iteration t
# for now, they just choose the closest lake. In future drafts, other variables
# will influence choices

angler.decisions<-function(fishery, t, y){
  lakeDistance<-fishery[["lakeDistance"]]
  anglerCharacteristics<-fishery[["anglerCharacteristics"]]
  anglerDecisions<-fishery[["anglerDecisions"]]
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  lakeStatus<-fishery[["lakeStatus"]]
  
  betaTravel<-parameters[["betaTravel"]]
  betaFish<-parameters[["betaFish"]]
  
  nDays<-parameters[["nDays"]]
  
  # remove old outputs. Results of previous loops go into lakeStatus
  anglerDecisions<-anglerDecisions[,c("anglerID","WBIC","catch","harvest")]
  anglerDecisions$WBIC<-rep(NA)
  anglerDecisions$catch<-rep(NA)
  anglerDecisions$harvest<-rep(NA)
  
  nAnglers<-parameters[["nAnglers"]]
  
  #add current fish population to lakeStatus df
  
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
 
  # This will eventually hold other lake characteristics relevant to decisions; catch rates, sizes, etc
   lakeDistancePop<-lakeDistance%>%
    dplyr::left_join(lakeStatusPrevious[,c("WBIC","fishN")], by="WBIC")%>%
     # starting with the same arbitrary coefficient values for everyone
     mutate(betaTravel=betaTravel,
            betaFish=betaFish)%>%
     mutate(expUtility=exp(distance*betaTravel+fishN*betaFish))%>%
     group_by(anglerID)%>%
     mutate(sumExpUtility=sum(expUtility))%>%
     ungroup()%>%
     mutate(probChoice=expUtility/sumExpUtility)
  
   # next step is sampling from lakes with probability from logit. Lake selection is not totally deterministic
   
   anglerDecisions<-lakeDistancePop%>%
     group_by(anglerID)%>%
     slice_sample(n=1, weight_by=probChoice)
   


  fishery[["anglerDecisions"]]<-anglerDecisions
  return(fishery)

}