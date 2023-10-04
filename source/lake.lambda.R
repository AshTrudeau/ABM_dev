# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.lambda<-function(parameters){
  nLakes<-parameters[["nLakes"]]
  lakeID<-seq(1:nLakes)
  
  catchParam<-sample(c(1,2,3,4), size=nLakes, replace=T)
  
  lakeCharacteristics<-data.frame(cbind(lakeID, catchParam))
}