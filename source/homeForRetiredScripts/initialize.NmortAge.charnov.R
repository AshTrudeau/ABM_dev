initialize.NmortAge.charnov<-function(parameters, fishSizes, selectLakes){
  
  # uhhhh these mortalities didn't look right to me; I'm not sure what went wrong. Pivoting to constant mortality for now
  allAges<-parameters[["allAges"]]
  nYears<-parameters[["nYears"]]
  nLakes<-parameters[["nLakes"]]
  
  # this natural mortality specification does not change through the years. But I'm saving this
  # matrix structure (rows by age, columns by year of simulation) so that it's easier to switch between them
  #NmortAge<-matrix(data=0, nrow=length(allAges), ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
  #NmortAge<-lapply(seq_len(nLakes), function(x) NmortAge)
  
  # now fill in NmortAge with calculation (for each age) using fishSizes list
  
  NmortVector<-lapply(fishSizes, function(x){
    x$k*(x$length/x$linf)^-1.5
  })
  
  NmortAge<-lapply(NmortVector, function(x){
    matrix(x, length(x), nYears, dimnames=list(as.character(allAges), 
                                               as.character(1:nYears)))
  })
  
  names(NmortAge)<-selectLakes$WBIC
  
  return(NmortAge)
  
}