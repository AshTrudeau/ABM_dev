ageing<-function(y, fishery, parameters){
  
  nYears<-parameters[["nYears"]]
  
  if(y<nYears){
    
  fishPop<-fishery[["fishPop"]]
  nYears<-parameters[["nYears"]]
  nAges<-parameters[["nAges"]]
  
  # move all fish up 1 age class in the next year.
  # age 0 recruits will be added by the recruitment function
  
  # for next year of loop
  thisYear<-unname(fishPop[,y+1])
  # add 0 for age 0 class to be filled in by recruitment
  nextYear.tail<-c(0, thisYear)
  # add 16 year old walleye back into 15+ class
  nextYear.tail[nAges+1]<-nextYear.tail[nAges+1]+nextYear.tail[nAges+2]
  nextYear<-nextYear.tail[1:c(nAges+1)]
  
  fishPop[,y+2]<-nextYear
  
  fishery[["fishPop"]]<-fishPop
  
  }
  return(fishery)
}