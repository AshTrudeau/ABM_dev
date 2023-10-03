# place anglers at randomly selected 'homes' on grid

angler.location<-function(parameters){
  nAnglers<-parameters[["nAnglers"]]
  edgeLength<-parameters[["edgeLength"]]
  
  x.a<-runif(nAnglers, min=0, max=edgeLength)
  y.a<-runif(nAnglers, min=0, max=edgeLength)
  anglerID<-seq(1:nAnglers)
  
  anglerLoc<-cbind.data.frame(anglerID, x.a, y.a)
  
}

