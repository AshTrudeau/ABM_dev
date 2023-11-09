
# this function makes a vector object that holds the year 1 day 1 starting population (N0),
# which is then updated in the update.lakes function
initialize.start.pop<-function(parameters){
  nAges<-parameters[["nAges"]]
  N0<-parameters[["N0"]]
  nYears<-parameters[["nYears"]]
  
  # make matrix that includes age 0
  startPop<-matrix(data=0, nrow=nAges+1, ncol=nYears)
  startPop[1,1]<-N0
  return(startPop)
}