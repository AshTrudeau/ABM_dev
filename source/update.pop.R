update.pop<-function(y, parameters, fishery, annualOutput){
  
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  
  thisYear<-annualOutput[annualOutput$year==y,]
  
  thisYear<-thisYear%>%
    left_join(lakeCharacteristics[,c("lakeID","c","rho","weightVulnerable")], by=c("lakeID"))%>%
    mutate(fishNEnd=survival*fishNStart+recruitment-annualHarvestN,
           fishBEnd=survival*(c*fishNStart+rho*fishBStart)+weightVulnerable*recruitment-annualHarvestB)%>%
    select(lakeID, year, fishNStart, fishBStart, annualHarvestN, annualHarvestB, annualEffort, M, 
           exploitation, survival, recruitment, fishNEnd, fishBEnd)
  
  annualOutput[annualOutput$year==y,]<-thisYear
  return(annualOutput)
  
}