# this script produces the catch and harvest for each fishing trip.
fishing<-function(fishery, parameters, t, y){
  
  # this df has the catch coefficients
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  # and this df has the fish populations as of the previous timestep
  lakeStatus<-fishery[["lakeStatus"]]
  

  # this df has the lake choice for each angler
  anglerDecisions<-fishery[["anglerDecisions"]]
  
  nAnglers<-parameters[["nAnglers"]]
  nLakes<-parameters[["nLakes"]]
  
  beta<-parameters[["beta"]]
  q<-parameters[["q"]]
  

  # for the first 'day' of the simulation
  if(t==1 & y==1){
    yesterdayFish<-lakeStatus%>%
      filter(day==0 & year==0)
  }
  # for the first day of any but the first year
  if(t==1 & y!=1){
    yesterdayFish<-lakeStatus%>%
      filter(day==nDays & year==y-1)
  # for every other day  
  }
  if(t!=1){
    
    yesterdayFish<-lakeStatus%>%
      filter(day==t-1 & year==y )
  }
  
  
 
  # assuming 4 hour fishing trips. Later, draw this from an empirical distribution
  # drawing catch and harvest for each angler. Add stochasticity later + nuance for harvest decision
  anglerDecisions<-anglerDecisions%>%
    # joining lakeCharacteristics, but without fishN0 (column 3)
    dplyr::left_join(yesterdayFish[,c("lakeID","fishN")], by="lakeID")%>%
    dplyr::mutate(catch=(q*fishN^beta)*4,
                  harvest=catch)

  # daily catch, effort, and harvest for each lake. Note that this will skip any lakes that don't have any effort.
  lakeHarvestToday<-anglerDecisions%>%
    dplyr::group_by(lakeID)%>%
    dplyr::summarize(harvestedN=sum(harvest),
                     nAnglers=n())%>%
    dplyr::left_join(lakeCharacteristics[,c("lakeID","meanWeight")], by="lakeID")%>%
    dplyr::mutate(harvestedB=harvestedN*meanWeight)%>%
    dplyr::ungroup()%>%
    dplyr::select(lakeID, harvestedN, harvestedB, nAnglers)
  
  lakeID<-data.frame(lakeID=seq(1:nLakes))
  
  lakeHarvestTodayZeroes<-lakeID%>%
    left_join(lakeHarvestToday, by="lakeID")%>%
    dplyr::mutate(across(.cols=c("harvestedN","harvestedB","nAnglers"), ~replace_na(.x, 0)))
  
  # lakeStatusToday fish population and biomass need to be updated, then added into lakeStatus by year nd day.
  # don't forget to write it into the fishery list
  
  lakeStatusToday<-lakeStatus%>%
    dplyr::filter(day==t & year==y)%>%
    dplyr::mutate(nAnglers=lakeHarvestTodayZeroes$nAnglers,
                  harvestedN=lakeHarvestTodayZeroes$harvestedN,
                  harvestedB=lakeHarvestTodayZeroes$harvestedB,
                  fishN=yesterdayFish$fishN-harvestedN,
                  fishB=yesterdayFish$fishB-harvestedB)
    
  lakeStatus[lakeStatus$day==t & lakeStatus$year==y,]<-lakeStatusToday

  fishery[["anglerDecisions"]]<-anglerDecisions[,c("anglerID","lakeID","catch","harvest")]
  fishery[["lakeStatus"]]<-lakeStatus
  return(fishery)
  

}