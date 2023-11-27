fish.size<-function(parameters, selectLakes){
  
  # eventually fishSize will be a nested list with one object per waterbody; meant to match fishPop object
  allAges<-parameters[["allAges"]]
  nLakes<-parameters[["nLakes"]]
  
  # not strictly necessary, but helpful for me. 
  growthParams<-selectLakes%>%
    dplyr::select(WBIC, linf, k, t0)
    # make into a list; 1 row in each object
  WBIC<-growthParams$WBIC
    
  growthParamsList<-split(growthParams, 1:nrow(growthParams))
  names(growthParamsList)<-selectLakes$WBIC
  
  # note that length is in centimeters and weight in kilograms, but DNR conversion assumes inches and pounds. need to convert
  fishSize<-data.frame(age=allAges,
                       length=rep(NA),
                       weight=rep(NA))
  
  fishSizeList<-lapply(seq_len(nLakes), function(x) fishSize)
  names(fishSizeList)<-WBIC
  
  # It may be simpler to add columns for linf, k, and t0 to the fishSizeList elements. Then I don't need to use two objects
  # for the vbgf function
  
  # this is kludgy but works; this and other solutions here https://stackoverflow.com/questions/13404877/adding-a-new-column-to-each-element-in-a-list-of-tables-or-data-frames
  fishSizeList<-mapply(cbind, fishSizeList, "linf"=growthParams$linf, SIMPLIFY=F)
  fishSizeList<-mapply(cbind, fishSizeList, "k"=growthParams$k, SIMPLIFY=F)
  fishSizeList<-mapply(cbind, fishSizeList, "t0"=growthParams$t0, SIMPLIFY=F)
  
  
# sweet victory, source: https://stackoverflow.com/questions/45317327/apply-function-to-columns-in-a-list-of-data-frames-and-append-results
  fishSizeList<-lapply(fishSizeList, function(x){
    x$length<-x$linf*(1-exp(-1*x$k*(x$age-x$t0)))
    x$weight<-(((x$length/2.54)^3)/2700)*0.454
    return(x)
  })  
  


  # for now making the bonkers assumption that fish don't grow after age 15
  
  # weight conversion from here https://dnr.wisconsin.gov/topic/Fishing/questions/estfishweight.html 
  
  return(fishSizeList)
  
}