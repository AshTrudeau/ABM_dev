# this script creates the 'blank' dataframe for angler decisions based on the model parameters

create.blank.angler.decisions<-function(parameters){
  
  nAnglers<-parameters[["nAnglers"]]
  
  blank.angler.decisions<-data.frame(matrix(nrow=nAnglers, ncol=4))
  
  colnames(blank.angler.decisions)<-c("anglerID","WBIC","catch","harvest")
  
  blank.angler.decisions$anglerID<-seq(1:nAnglers)
  
  return(blank.angler.decisions)
}