# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.characteristics<-function(parameters){
  nLakes<-parameters[["nLakes"]]
  nFish0_min<-parameters[["nFish0_min"]]
  lakeID<-seq(1:nLakes)
  
  # currently starting with fish populations that scale directly to catchParam. This is temporary
  # and will be replaced by fish population model and explicit catch equation with a hyperstability parameter
  fishPop0<-rpois(nLakes, 1000)
  fishPop<-fishPop0
  
  lakeCharacteristics<-data.frame(cbind(lakeID, fishPop0, fishPop))
  
}