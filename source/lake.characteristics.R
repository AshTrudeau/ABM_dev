# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.characteristics<-function(lakes, parameters){
  
  alpha<-parameters[["alpha"]]
  beta<-parameters[["beta"]]
  sigma<-parameters[["sigma"]]

  lakeCharacteristics<-lakes%>%
    dplyr::select(WBIC, `Final Lake Class`, `Area (ha)`)%>%
    dplyr::rename("lakeID"=WBIC,
           "lakeClass"=`Final Lake Class`,
           "areaHa"=`Area (ha)`)%>%
    dplyr::mutate(alpha=alpha,
                  beta=beta,
                  sigma=sigma)
    
    return(lakeCharacteristics)
  
  
}