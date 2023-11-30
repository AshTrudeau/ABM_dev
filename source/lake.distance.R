# lake.distance()
# straight-line distance from each lake to each angler. Other lake characteristics must
# be put in the lakeCharacteristics element of the list.

lake.distance<-function(lakeCharacteristics, anglerCharacteristics){
  anglerID<-anglerCharacteristics$anglerID
  WBIC<-lakeCharacteristics$WBIC
  
  all.combinations<-tidyr::expand_grid(anglerID, WBIC)%>%
    dplyr::left_join(anglerCharacteristics, by="anglerID")%>%
    dplyr::left_join(lakeCharacteristics, by="WBIC")%>%
    # convert degrees to km
    dplyr::mutate(distance=sqrt((lakeLong-anglerLong)^2+(lakeLat-anglerLat)^2)*111)
  
}

