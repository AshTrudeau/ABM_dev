# this function creates the lake landscape. 

lake.location<-function(parameters, workingList){
  nLakes<-parameters[["nLakes"]]
  edgeLength<-parameters[["edgeLength"]]
  
  # randomly selected points for lakes
  x<-runif(nLakes, min=0, max=edgeLength)
  y<-runif(nLakes, min=0, max=edgeLength)
  lakeID<-seq(1:nLakes)
  
  #  these are the coordinates of each of the n lakes
  lake.location<-cbind.data.frame(lakeID, x, y)
  
}