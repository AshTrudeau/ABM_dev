natural.mortality<-function(y, parameters, fishery){
  # later I'll adjust M based on F (negative relationship)
  M<-parameters[["M"]]
  NmortAge<-fishery[["NmortAge"]]
  fishPop<-fishery[["fishPop"]]
  
  # fishPop starts on year 0, NmortAge on year 1
  NmortAge[,y]<-fishPop[,y]*M
  
  # still in current year
  fishPop[,y+1]<-fishPop[,y+1]-NmortAge[,y]
  fishPop[,y+1]<-ifelse(fishPop[,y+1]<0, 0, fishPop[,y+1])
  
  fishery[["fishPop"]]<-fishPop
  fishery[["NmortAge"]]<-NmortAge
  return(fishery)
  
}