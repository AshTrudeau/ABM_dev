update.lakes<-function(y, fishery, parameters){
 
   nYears<-parameters[["nYears"]]
  
  # stop script  if last year
  
  if(y<nYears){
    
    lakeStatus<-fishery[["lakeStatus"]]
    fishPops<-fishery[["fishPops"]]
    startPops<-fishery[["startPops"]]
    
    # update lakeStatus for day 1 of the next year
    
    thisYear<-annualOutput[annualOutput$year==y,]
    
    nextYear.day1<-lakeStatus[lakeStatus$year==y+1 & lakeStatus$day==1,]
    
    nextYear.day1$fishN<-thisYear$fishNEnd

    lakeStatus[lakeStatus$year==y+1 & lakeStatus$day==1,]<-nextYear.day1
    
    fishery[["lakeStatus"]]<-lakeStatus
    
    # update startPops with aged fish
    
    fishPops.to.move<-lapply(fishPops, function(x) x[,y+2])
    
    startPops<-lapply(seq_along(startPops), function(x){
      lake_matrix<-startPops[[x]]
      lake_name<-names(startPops)[x]
      lake_vector<-fishPops.to.move[[lake_name]]
      
      lake_matrix[,y+1]<-lake_vector
      return(lake_matrix)
    })
    

    fishery[["startPops"]]<-startPops
    

 
  }
   return(fishery)
   
  
  
}