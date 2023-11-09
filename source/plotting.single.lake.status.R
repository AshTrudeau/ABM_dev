plotting.single.lake.status<-function(annualOuput, fishery, parameters){
  
  annualOutput<-annualOutput%>%
    mutate(Fmort=annualHarvestN/fishNStart)
  
  
  ggplot(annualOutput)+
    geom_line(aes(x=year, y=fishNEnd))+
    theme_bw()
  
  ggplot(annualOutput)+
    geom_line(aes(x=year, y=Fmort))+
    theme_bw()
  
  ggplot(annualOutput)+
    geom_line(aes(x=year, y=annualHarvestN))+
    theme_bw()
  
}