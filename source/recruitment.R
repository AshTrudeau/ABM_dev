recruitment<-function(y, parameters, fishery, annualOutput){
  # take out this loop's year of output for e ach lake
  
  # this is all out of order; come back to it
  lastYear<-annualOutput[annualOutput$year==y-1]
  thisYear<-annualOutput[annualOutput$year==y,]
  
  # Recruitment--random walk with error; a different value
  # for each lake
  
  # Let's say the mean recruitment is 75
  
  # first a starting value for each lake  (time step 0,)
  
  if(y==1){
    R0<-rnorm(nLakes, mean=75, sd=25)
    
    thisYear$recruitment<-sum(R0, rnorm(nLakes, mean=0, sd=25))
  }else{
    thisYear$recruitment<-sum(thisYear$)
  }
  
  # and each year's loop adds the  next step for each lake
  
  thisYear$recruitment<-
  
  # and put it back in
  
  annualOutput[annualOutput$year==y,]<-thisYear
  return(annualOutput)
}