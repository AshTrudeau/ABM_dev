# lake.distance()
# straight-line distance from each lake to each angler

lake.distance<-function(lakeLocation, anglerLocation){
  anglerID<-anglerLocation$anglerID
  lakeID<-lakeLocation$lakeID
  
  all.combinations<-tidyr::expand_grid(anglerID, lakeID)%>%
    dplyr::left_join(anglerLocation, by="anglerID")%>%
    dplyr::left_join(lakeLocation, by="lakeID")%>%
    dplyr::mutate(distance=sqrt((x-x.a)^2+(y-y.a)^2))
  
}

