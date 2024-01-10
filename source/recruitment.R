recruitment<-function(y, fishery, parameters, burnin){
  
  # stop recruitment on the last year of the simulation
  
  nBurnIn<-parameters[["nBurnIn"]]
  
  if(burnin==TRUE){
    nYears<-nBurnIn
  } else{
    nYears<-parameters[["nYears"]]
  }
  
  nAges<-parameters[["nAges"]]
  
  if(y<nYears){
  
  ageMature<-parameters[["ageMature"]]
  fishPops<-fishery[["fishPops"]]
  # has alpha, beta, sigma regional values (same for each lake, but leave room for lake-specific
  # values)
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  # selecting regional alpha and beta from Tsehaye et al 2016
  

  nextYear<-lapply(fishPops, function(x) x[,y+1])
  
  # find fish population above age of maturity
  
  mature<-lapply(nextYear, function(x) x[1:c(nAges+1)>ageMature])
  
  stock<-lapply(mature, function(x) sum(x))
  
  divide<-function(x,y){
    x/y
  }
  
  # find mature walleye per hectare
  stockDensity<-mapply(divide, stock, as.list(lakeCharacteristics$areaHa))
  stockDensity<-as.list(stockDensity)
  
  # calculate recruits
  #  removing stochasticity
  # recruit.fun<-function(stockDensity, recAlpha, recBeta, recSigma){
  #   recruitsHa<-(recAlpha*stockDensity*exp(-recBeta*stockDensity))*exp(rnorm(n=length(stockDensity),
  #                                                                            mean=0, sd=recSigma^2))
  # }
  
  recruit.fun<-function(stockDensity, recAlpha, recBeta){
    recruitsHa<-(recAlpha*stockDensity*exp(-recBeta*stockDensity))
    
  }
  
  recAlpha<-as.list(lakeCharacteristics$recAlpha)
  recBeta<-as.list(lakeCharacteristics$recBeta)
  recSigma<-as.list(lakeCharacteristics$recSigma)
  
  #recruitsHa<-mapply(recruit.fun, stockDensity, recAlpha, recBeta, recSigma)
  recruitsHa<-mapply(recruit.fun, stockDensity, recAlpha, recBeta)
  
  recruit.lake<-function(recruitsHa, areaHa){
    round(recruitsHa*areaHa)
  }
  
  recruitsLake<-mapply(recruit.lake, recruitsHa, as.list(lakeCharacteristics$areaHa))
  recruitsLake<-as.list(recruitsLake)
  
  # add age 0 recruits to next year of population matrix
  
  fishPops<-lapply(seq_along(fishPops), function(x){
    lake_matrix<-fishPops[[x]]
    lake_name<-names(fishPops)[x]
    lake_vector<-recruitsLake[[lake_name]]
    
    lake_matrix[1,y+1]<-lake_vector
    return(lake_matrix)
  })
  names(fishPops)<-lakeCharacteristics$WBIC
  
  fishery[["fishPops"]]<-fishPops
  }
  
  return(fishery)
}