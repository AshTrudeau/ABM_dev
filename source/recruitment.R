recruitment<-function(y, fishery, parameters){
  
  # stop recruitment on the last year of the simulation
  
  nYears<-parameters[["nYears"]]
  
  if(y<nYears){
  
  ageMature<-parameters[["ageMature"]]
  fishPop<-fishery[["fishPop"]]
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  # selecting regional alpha and beta from Tsehaye et al 2016
  
  alpha<-lakeCharacteristics$alpha
  beta<-lakeCharacteristics$beta
  sigma<-lakeCharacteristics$sigma
  
  
  # 'next year's population matrix--filling in recruits at age 0
  nextYear<-fishPop[,y+2]
  # stock is fish population where age is above age of maturity
  stock<-sum(nextYear[1:length(nextYear)>ageMature])
  # mature walleye per hectare
  stockDensity<-stock/lakeCharacteristics$areaHa
  
  recruitsHa<-alpha*stockDensity*exp(-beta*stockDensity)+rnorm(n=length(stockDensity),mean=0, sd=sigma)
  recruitsHa<-ifelse(recruitsHa<0, 0, recruitsHa)
  
  recruits<-round(recruitsHa*lakeCharacteristics$areaHa)
  # add age 0 recruits to population matrix
  fishPop[1,y+2]<-recruits
  fishery[["fishPop"]]<-fishPop
  }
  
  return(fishery)
}