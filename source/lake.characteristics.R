# selecting and appending relevant lake characteristics:
# area (ha), class, VBGF params, recruitment params

lake.characteristics<-function(parameters, selectLakes, vbgf_lakeClass, 
                                vbgf_lakeSpecific){
  
  nLakes<-parameters[["nLakes"]]
  recAlpha<-parameters[["recAlpha"]]
  recBeta<-parameters[["recBeta"]]
  recSigma<-parameters[["recSigma"]]
  
  `%!in%`<-Negate(`%in%`)
  
  
  lakeGrowth<-selectLakes%>%
    filter(lakeSpecificGrowth==1)%>%
    left_join(vbgf_lakeSpecific, by="WBIC")
  
  classGrowth<-selectLakes%>%
    filter(lakeSpecificGrowth==0)%>%
    left_join(vbgf_lakeClass, by="lakeClass")
  

  selectLakesGrowth<-rbind.data.frame(lakeGrowth, classGrowth)
  
  lakeCharacteristics<-selectLakesGrowth%>%
    dplyr::select(WBIC, County, Latitude, Longitude, lakeClass, linf, k, t0, `Area (ha)`)%>%
    dplyr::rename("county"=County,
                  "lakeLat"=Latitude,
                  "lakeLong"=Longitude,
                  "areaHa"=`Area (ha)`)%>%
    # now arrange rows to be in the same order as selectLakes
    dplyr::slice(match(selectLakes$WBIC, WBIC))
  
  lakeCharacteristics$recAlpha<-rep(recAlpha)
  lakeCharacteristics$recBeta<-rep(recBeta)
  lakeCharacteristics$recSigma<-rep(recSigma)
  
  return(lakeCharacteristics)
  
}