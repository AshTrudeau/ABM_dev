# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.characteristics<-function(parameters, lakeClasses.c.rho.df, lakeClasses.length.weight){
  nLakes<-parameters[["nLakes"]]
  nFish0_min<-parameters[["nFish0_min"]]
  lakeID<-seq(1:nLakes)
  
  # lakeClasses.c.rho.df<-read_csv(here::here(base.directory,"data","walleye.lakeClass.length.weight.lm.csv"))%>%
  #   rename("lakeClasses"="LakeClass")
  # lakeClasses.length.weight<-read_csv(here::here(base.directory, "data", "walleye.lakeClass.length.weight.csv"))
  
  # currently starting with fish populations that scale directly to catchParam. This is temporary
  # and will be replaced by fish population model and explicit catch equation with a hyperstability parameter
  
  
  fishPop0<-rpois(nLakes, 1000)
  fishPop<-fishPop0
  
  lakeClasses<-sample(lakeClasses.c.rho.df$lakeClasses, nLakes, replace=T)
  
  lakeCharacteristics<-data.frame(cbind(lakeID, lakeClasses, fishPop0, fishPop))
  
  # add on c and rho for fish pop models
  
  lakeCharacteristics.c.rho<-lakeCharacteristics%>%
    left_join(lakeClasses.c.rho.df[,-1], by="lakeClasses")
  
  return(lakeCharacteristics.c.rho)
  
  
}