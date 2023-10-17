# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, parameters, t, y){
  
  # this df has the catch coefficients
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # and this df has the fish populations as of the previous timestep
  lakeStatus<-fishery[["lakeStatus"]]
  

  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  
  nAnglers<-parameters[["nAnglers"]]
  
  beta<-parameters[["beta"]]
  q<-parameters[["q"]]
  

  if(t==1 & y==1){
    yesterdayFish<-lakeStatus%>%
      filter(day==0 & year==0)
  }
  
  if(t==1 & y!=1){
    yesterdayFish<-lakeStatus%>%
      filter(day==nDays & year==y-1)
    
  }
  if(t!=1 & y!=1){
    
    yesterdayFish<-lakeStatus%>%
      filter(day==t-1 & year==y )
  }
  
  
 
  # assuming 4 hour fishing trips. Later, draw this from an empirical distribution
  
  anglerDecisions<-anglerDecisions%>%
    # joining lakeCharacteristics, but without fishN0 (column 3)
    dplyr::left_join(yesterdayFish[,c("lakeID","fishN")], by="lakeID")%>%
    dplyr::mutate(catch=(q*fishN^beta)*4,
                  harvest=catch)

  lakeHarvestToday<-anglerDecisions%>%
    dplyr::group_by(lakeID)%>%
    dplyr::summarize(harvestedN=sum(harvest),
                     nAnglers=n())%>%
    dplyr::left_join(lakeCharacteristics[,c("lakeID","meanWeight")], by="lakeID")%>%
    dplyr::mutate(harvestedB=harvestedN*meanWeight)%>%
    dplyr::ungroup()%>%
    dplyr::select(lakeID, harvestedN, harvestedB, nAnglers)
  
  # this is where I left off; I need to fill in any lakes that were not visited so the lengths match. 
  # lakeStatusToday fish population and biomass need to be updated, then added into lakeStatus by year nd day.
  # don't forget to write it into the fishery list
  
  lakeStatusToday<-lakeStatus%>%
    dplyr::filter(day==t & year==y)
    dplyr::mutate(nAnglers=lakeHarvestToday$nAnglers,
                  harvestedN=lakeHarvestToday$harvestedN,
                  harvestedB=lakeHarvestToday$harvestedB)
    
  
  lakeCharacteristics<-lakeCharacteristics%>%
    dplyr::left_join(lakeHarvest, by="lakeID")%>%
    # not every lake is visited each loop; fil in NA values with 0
    dplyr::mutate(harvestedN=ifelse(is.na(harvestedN), 0, harvestedN),
                  harvestedB=ifelse(is.na(harvestedB), 0, harvestedB),
                  fishN=fishN-harvestedN,
                  fishB=fishB-harvestedB)
  
  fishery[["anglerDecisions"]]<-anglerDecisions[,c("anglerID","lakeID","catch","harvest")]
  fishery[["lakeCharacteristics"]]<-lakeCharacteristics[,c("lakeID","lakeClasses","meanWeight","fishN0","fishN","fishB0","fishB","harvestedN","harvestedB")]
  return(fishery)
  

}