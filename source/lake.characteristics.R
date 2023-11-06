# This is a temporary placeholder for the scripts that will define the lake characteristics.
# for now, each lake is randomly assigned (from 1:4) a Poisson distribution coefficient
# from which catch is drawn. 

lake.characteristics<-function(lakes, parameters){

  # nope, that doesn't work
  # get ALK for lake
  
  # get fish age data from fmdb
# ageLengthData<-get_fmdb_lenage(wbic=lakes$WBIC)%>%
#     # change this when I start using other species
#     filter(species%in%c("walleye"))%>%
#     select(species, county, wbic, year, age, length)%>%
#     dplyr::rename("spp"=species,
#                   "waterbody"=wbic)

# this returns a tibble with the species and waterbody specific ALK, as well as the species-specific ALK  
# look into matching lakes with their HUC 10 for lakes with no age length data
# later I can add other lake characteristics to this tibble
#lakeCharacteristics<-make_halk(ageLengthData, levels=c("spp","waterbody"), plus_group=15, min_age=1)
  
  alpha<-parameters[["alpha"]]
  beta<-parameters[["beta"]]
  sigma<-parameters[["sigma"]]
  
  lakeCharacteristics<-lakes%>%
    dplyr::select(WBIC, `Final Lake Class`)%>%
    dplyr::rename("lakeID"=WBIC,
           "lakeClass"=`Final Lake Class`)%>%
    dplyr::mutate(alpha=alpha,
                  beta=beta,
                  sigma=sigma)
  
  

  return(lakeCharacteristics)
  
  
}