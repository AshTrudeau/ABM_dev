select.lakes<-function(parameters, allLakes, drawProp){
  
  nLakes<-parameters[["nLakes"]]
  
  selectLakes<-allLakes%>%
    group_by(lakeClass)%>%
    mutate(nClass=n())%>%
    ungroup()%>%
    mutate(totalLakes=length(unique(WBIC)),
           probSample=nClass/totalLakes)%>%
    slice_sample(n=nLakes, weight_by=probSample)
  
  return(selectLakes)
  
  }