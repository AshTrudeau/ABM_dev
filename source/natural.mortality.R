natural.mortality<-function(y, parameters, fishery){
  # M gives 'base' natural mortality that will be adjusted according to annual exploitation
  # multiply age-specific M by annual M predicted by annual u/F to get that year's age specific M
  M<-parameters[["M"]]
  ageVulnerable<-parameters[["ageVulnerable"]]
  NmortAge<-fishery[["NmortAge"]]
  startPop<-fishery[["startPop"]]
  harvestAge<-fishery[["harvestAge"]]
  fishPop<-fishery[["fishPop"]]
  
  harvestedThisYear<-sum(harvestAge[,y])
  startPopThisYear<-startPop[c(ageVulnerable+1):nrow(startPop),y]
  
  if(harvestedThisYear!=0){
    exploitation<-harvestedThisYear/sum(startPopThisYear)
    # this relationship is from Hansen et al 2011 NAJFM, Escanaba study
    NmortAge[,y]<-(0.7-0.92*exploitation)*M
  } else {
    NmortAge[,y]<-M
  }
  
  # instance of negative natural mortality during a very high exploitation year. Set a floor
  NmortAge[,y]<-ifelse(NmortAge[,y]<0, 0.01, NmortAge[,y])
  

  # still in current year
  fishPop[,y+1]<-fishPop[,y+1]-(NmortAge[,y]*fishPop[,y+1])
  fishPop[,y+1]<-ifelse(fishPop[,y+1]<0, 0, fishPop[,y+1])
  
  fishery[["fishPop"]]<-fishPop
  fishery[["NmortAge"]]<-NmortAge
  return(fishery)
  
}