annual.output<-function(y, fishery, annualOutput){
  lakeStatus<-fishery[["lakeStatus"]]
  fishPop<-fishery[["fishPop"]]
  
  # don't use lakeStatus for end of year fish population! It hasn't been updated with natural mortality yet!
  
  # i'll need to set this up for multiple lakes. Take in list of fishPop matrices, put out vector of end of year
  # fish populations
  fishNEnd<-sum(fishPop[,y+1])
  
  # effort isn't set up yet; doing that once I have multiple lakes
  lakeStatusYear<-lakeStatus%>%
    filter(year==y)%>%
    group_by(lakeID)%>%
    summarize(fishNStart=fishN[which.min(day)]+harvestedN[which.min(day)],
              annualHarvestN=sum(harvestedN),
              annualEffort=sum(nAnglers),
              year=unique(year))%>%
    ungroup()%>%
    mutate(fishNEnd=fishNEnd)%>%
    select(lakeID, year, fishNStart, fishNEnd, annualHarvestN, annualEffort)
  
  annualOutput[annualOutput$year==y,]<-lakeStatusYear
  
  return(annualOutput)
  

}