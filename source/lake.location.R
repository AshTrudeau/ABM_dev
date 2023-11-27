# this function creates the lake landscape. 

lake.location<-function(parameters, selectLakes){
  nLakes<-parameters[["nLakes"]]
  
  lakeLocation<-selectLakes%>%
    dplyr::select(WBIC, County, Latitude, Longitude)%>%
    dplyr::rename("county"=County,
           "lakeLat"=Latitude,
           "lakeLong"=Longitude)
  return(lakeLocation)

}