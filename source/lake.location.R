# this function creates the lake landscape. 

lake.location<-function(parameters, lakes){
  nLakes<-parameters[["nLakes"]]
  
  lakeLocation<-lakes%>%
    dplyr::select(WBIC, County, Latitude, Longitude)%>%
    dplyr::rename("lakeID"=WBIC,
           "county"=County,
           "lakeLat"=Latitude,
           "lakeLong"=Longitude)
  return(lakeLocation)

}