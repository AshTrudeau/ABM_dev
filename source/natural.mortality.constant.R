natural.mortality.constant<-function(y, fishery, parameters, burnin){
  # M gives 'base' natural mortality that will be adjusted according to annual exploitation
  # multiply age-specific M by annual M predicted by annual u/F to get that year's age specific M

  startPops<-fishery[["startPops"]]
  fishPops<-fishery[["fishPops"]]
  NmortAge<-fishery[["NmortAge"]]
  
  wbics<-names(fishPops)
  
  fishPopsYear<-mapply(function(a,b,c) a[,y]-(b[,y]*c[,y]),
                       a=fishPops,
                       b=NmortAge,
                       c=startPops)
  
  fishPopsYear<-as.list(as.data.frame(fishPopsYear))
  
  # replace any negative values with zeroes
  fishPopsYear<-lapply(fishPopsYear, function(x){
    x[x<0]<-0; x
  })
  
  #  now replace this year's column in fishPops with new values
  fishPops<-lapply(seq_along(fishPops), function(x){
    lake_matrix<-fishPops[[x]]
    lake_name<-names(fishPopsYear)[x]
    lake_vector<-fishPopsYear[[lake_name]]
    
    lake_matrix[,y]<-lake_vector
    return(lake_matrix)
  })
  
  names(fishPops)<-wbics 
  
  fishery[["fishPops"]]<-fishPops
  fishery[["NmortAge"]]<-NmortAge
  return(fishery)
  
}