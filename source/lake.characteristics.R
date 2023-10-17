# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.characteristics<-function(parameters, lakeClasses.c.rho.df, lakeClasses.length.weight){
  nLakes<-parameters[["nLakes"]]
  nFish0_min<-parameters[["nFish0_min"]]
  lakeID<-seq(1:nLakes)
  
  fishN0<-rpois(nLakes, 1000)

  lakeClasses<-sample(lakeClasses.c.rho.df$lakeClasses, nLakes, replace=T)
  
  lakeCharacteristics<-cbind.data.frame(lakeID, lakeClasses, fishN0=fishN0)
  
  # add on c and rho and weight vulnerable for fish pop models
  
  lakeCharacteristics.c.rho<-lakeCharacteristics%>%
    left_join(lakeClasses.c.rho.df[,-1], by="lakeClasses")%>%
    mutate(fishB0=fishN0*meanWeight)
  
  return(lakeCharacteristics.c.rho)
  
  
}