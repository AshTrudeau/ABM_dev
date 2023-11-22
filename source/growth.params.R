growth.params<-function(selectLakes, vbgf_lakeClass, vbgf_lakeSpecific){
  
  `%!in%`<-Negate(`%in%`)
  
  specificGrowth<-selectLakes%>%
    inner_join(vbgf_lakeSpecific, by="WBIC")
  
  classGrowth<-selectLakes%>%
    filter(WBIC%!in%specificGrowth$WBIC)%>%
    left_join(vbgf_lakeClass, by="lakeClass")
  
  selectLakes<-rbind.data.frame(specificGrowth, classGrowth)
  return(selectLakes)
  
}
