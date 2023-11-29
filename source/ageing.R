ageing<-function(y, fishery, parameters){
  

  if(y<nYears){
    
  fishPops<-fishery[["fishPops"]]
  nYears<-parameters[["nYears"]]
  nAges<-parameters[["nAges"]]
  
  # move all fish up 1 age class in the next year.
  # age 0 recruits will be added by the recruitment function
  
  # Take out columns from this year of fishPops
  thisYear<-lapply(fishPops, function(x) x[,y+1])
  thisYear<-lapply(thisYear, unname)
  
  # add 0 for age 0 class to be filled in by recruitment
  # append 0 as the first position of each of the fishPops elements
  
  concat.age.0<-function(x){
    c(0, x)
  }
  
  nextYear.head<-lapply(thisYear, concat.age.0)
  
  # add 16 year old walleye back into the 15+ age class, then drop the 16 year age class
  loop.age<-function(x){
    x[nAges+1]<-x[nAges+2] + x[nAges+1]
    x<-x[-c(nAges+2)]
  }
  
  nextYear.loop<-lapply(nextYear.head, loop.age)
  
  # now put that new column into position y+2 in fishPops
  
  wbics<-names(fishPops)

  fishPops<-lapply(seq_along(fishPops), function(x){
    lake_matrix<-fishPops[[x]]
    lake_vector<-nextYear.loop[[x]]
    lake_matrix[,y+2]<-lake_vector
    return(lake_matrix)
  })
  
  names(fishPops)<-wbics

  fishery[["fishPops"]]<-fishPops
  
  }
  return(fishery)
}