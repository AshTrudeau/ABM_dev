recruitment<-function(y, parameters, fishery, annualOutput){
  # no SR relationship yet; random walk with error
  # deal with year 1
  thisYear<-annualOutput[annualOutput$year==y,]
  
  if(y==1){
    R0<-rnorm(nLakes, mean=75, sd=25)
    
    thisYear$recruitment<-R0 + rnorm(nLakes, mean=0, sd=25)
    thisYear$recruitment<-ifelse(thisYear$recruitment<0,0,thisYear$recruitment)
    
  }else{
    lastYear<-annualOutput[annualOutput$year==y-1,]
    
    thisYear$recruitment<-lastYear$recruitment + rnorm(nLakes, mean=0, sd=25)
    thisYear$recruitment<-ifelse(thisYear$recruitment<0,0,thisYear$recruitment)
    
  }
  


  # and put it back in
  
  annualOutput[annualOutput$year==y,]<-thisYear
  return(annualOutput)
}