initialize.Nmort.constant<-function(parameters, fishSizes, selectLakes){
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  nLakes<-parameters[["nLakes"]]
  
  # this natural mortality specification does not change through the years. But I'm saving this
  # matrix structure (rows by age, columns by year of simulation) so that it's easier to switch between them
  #NmortAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
  #NmortAge<-lapply(seq_len(nLakes), function(x) NmortAge)
  
  # now fill in NmortAge with calculation (for each age) using fishSizes list
  
  NmortVector<-lapply(fishSizes, function(x){
    1-exp(-x$k*1.5)
  })
  
  NmortAge<-lapply(NmortVector, function(x){
    matrix(x, length(x), nYears, dimnames=list(as.character(allAges), 
                                               as.character(1:nYears)))
  })
  
  names(NmortAge)<-selectLakes$WBIC
  
  return(NmortAge)
  
}