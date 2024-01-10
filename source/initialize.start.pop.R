
# this function makes a vector object that holds the year 1 day 1 starting population (N0),
# which is then updated in the update.lakes function
initialize.start.pop<-function(parameters, fishery){
  # Before the first time step, startPops and fishPops look the same
  
  nAges<-parameters[["nAges"]]
  nYears<-parameters[["nYears"]]
  nBurnIn<-parameters[["nBurnIn"]]
  nLakes<-parameters[["nLakes"]]
  
  oldStartPops<-fishery[["startPops"]]
  
  # starting new startPops list of matrices with 1 blank startPop matrix
  startPopBlank<-matrix(data=0, nrow=nAges+1, ncol=nYears, dimnames=list(c(0:nAges), c(1:nYears)))
  
  
  #  take last column from (old) startPops, put it in the first  column of (new) startPops
  
  startPops<-lapply(seq_len(nLakes), function(x) startPopBlank)
  
  names(startPops)<-selectLakes$WBIC
  
  # put in pasting here
  # take out last year of burnin
  
  newStartPop<-lapply(oldStartPops, function(x) x[,nBurnIn])
  
  # insert each of these columns into their appropriate matrix in the startPops list
  
  startPops<-lapply(seq_along(startPops), function(x){
    lake_matrix<-startPops[[x]]
    lake_name<-names(startPops)[x]
    lake_vector<-newStartPop[[lake_name]]
    
    lake_matrix[,1]<-lake_vector
    return(lake_matrix)
  })
  
  names(startPops)<-names(newStartPop)
  
  fishery[["startPops"]]<-startPops
  
  return(fishery)
  
}