update.lakes<-function(y, fishery, parameters){
 
   nYears<-parameters[["nYears"]]
  
  # stop script  if last year
  
  if(y<nYears){
    
    lakeStatus<-fishery[["lakeStatus"]]
    
    thisYear<-annualOutput[annualOutput$year==y,]
    
    nextYear.day1<-lakeStatus[lakeStatus$year==y+1 & lakeStatus$day==1,]
    
    nextYear.day1$fishN<-thisYear$fishNEnd
   # nextYear.day1$fishB<-thisYear$fishBEnd
    
    lakeStatus[lakeStatus$year==y+1 & lakeStatus$day==1,]<-nextYear.day1
    
    fishery[["lakeStatus"]]<-lakeStatus
    
 
  }
   return(fishery)
   
  
  
}