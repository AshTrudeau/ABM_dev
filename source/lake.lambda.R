# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.lambda<-function(parameters){
  nLakes<-parameters[["nLakes"]]
  nFish0_min<-parameters[["nFish0_min"]]
  lakeID<-seq(1:nLakes)
  
  catchParam<-sample(c(1,2,3,4), size=nLakes, replace=T)
  
  # currently starting with fish populations that scale directly to catchParam. This is temporary
  # and will be replaced by fish population model and explicit catch equation with a hyperstability parameter
  fishPop0<-catchParam*nFish0_min
  fishPop<-fishPop0
  
  lakeCharacteristics<-data.frame(cbind(lakeID, catchParam, fishPop0, fishPop))
  
}