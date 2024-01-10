update.fishPops<-function(y, fishery, parameters, burnin){
  # currently pasted from update.lakes.R, update for this use
  
  nBurnIn<-parameters[["nBurnIn"]]
  
  if(burnin==TRUE){
    nYears<-nBurnIn
  } else{
    nYears<-parameters[["nYears"]]
  }
  

  # stop script  if last year
  
  if(y<nYears){
    
    fishPops<-fishery[["fishPops"]]
    # startPops already has year 0 from initializing. At end of year 1 (until and including the next to 
    # last year, update startPops with new year's starting population by age)
    startPops<-fishery[["startPops"]]


    # update startPops with aged fish
    
    fishPops.to.move<-lapply(fishPops, function(x) x[,y+1])
    
    startPops<-lapply(seq_along(startPops), function(x){
      lake_matrix<-startPops[[x]]
      lake_name<-names(startPops)[x]
      lake_vector<-fishPops.to.move[[lake_name]]
      
      lake_matrix[,y+1]<-lake_vector
      return(lake_matrix)
    })
    
    names(startPops)<-lakeCharacteristics$WBIC
    
    fishery[["startPops"]]<-startPops
    

  }
  return(fishery)
  
  
}