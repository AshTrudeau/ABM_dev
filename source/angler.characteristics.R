# place anglers at randomly selected 'homes' on grid. Eventually this function will also
# add additional angler characteristics

angler.characteristics<-function(parameters){
  nAnglers<-parameters[["nAnglers"]]
  minLat<-parameters[["minLat"]]
  minLong<-parameters[["minLong"]]
  maxLat<-parameters[["maxLat"]]
  maxLong<-parameters[["maxLong"]]
  
  anglerLat<-runif(nAnglers, min=minLat, max=maxLat)
  anglerLong<-runif(nAnglers, min=minLong, max=maxLong)
  
  anglerID<-seq(1:nAnglers)
  
  anglerCharacteristics<-cbind.data.frame(anglerID, anglerLat, anglerLong)
  
}

