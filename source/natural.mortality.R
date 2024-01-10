natural.mortality<-function(y, fishery, parameters, burnin){
  # M gives 'base' natural mortality that will be adjusted according to annual exploitation
  # multiply age-specific M by annual M predicted by annual u/F to get that year's age specific M
  M<-parameters[["M"]]
  ageVulnerable<-parameters[["ageVulnerable"]]
  
  #NmortAge<-fishery[["NmortAge"]]
  startPops<-fishery[["startPops"]]
  harvestAge<-fishery[["harvestAge"]]
  fishPops<-fishery[["fishPops"]]
  
  nAges<-parameters[["nAges"]]
  ages<-c(0:nAges)
  agesVulnerable<-as.character(ages[ages>=ageVulnerable])
  
  # get total number of fish harvested this year by lake and age
  # need to switch this off for burn-in. 
  if(burnin==FALSE){
  harvestAgeYear<-lapply(harvestAge, function(x) x[,y])
  } else{
    # fill them with 0s (really just taking first column, which is already full of zeroes)
    harvestAgeYear<-lapply(harvestAge, function(x) x[,1])
  }
 
   # get starting population by lake and age
  startPopsAgeYear<-lapply(startPops, function(x) x[,y])
  
  # name the vectors by age to make the next step easier
  startPopsAgeYear<-lapply(startPopsAgeYear, setNames, as.character(0:nAges))
  
  # set up reverse %in%
  `%!in%`<-Negate(`%in%`)
  
  # set up replacement function
  
  replaceAge<-function(startPopsAgeYear, agesVulnerable){
    startPopsVulnerable<-startPopsAgeYear
    startPopsVulnerable[names(startPopsVulnerable)%!in%agesVulnerable]<-0
    return(startPopsVulnerable)
  }

  if(ageVulnerable==0){
    startPopsVulnerable<-startPopsAgeYear
    # replace invulnerable age populations with 0
  } else{
    
    startPopsVulnerable<-lapply(startPopsAgeYear, replaceAge, agesVulnerable)
    
  }
  

# if no fish were harvested, only baseline natural mortality applies, otherwise
# it's a function of natural mortality and exploitation rate
    # exploitation rate is N harvested over total vulnerable
  divide<-function(x, y){
    x/y
  }
  # for fishing mortality i used a for loop. see if I can vectorize it instead (and go back if I'm successful)
  exploitation<-mapply(divide, harvestAgeYear, startPopsVulnerable)
  # replace NAN (divided by zero) with 0
  exploitation[is.nan(exploitation)]<-0
  # make it back into a list
  exploitation<-as.list(as.data.frame(exploitation))

  # NmortAge is currently blank; need to insert NmortAgeYear once it's calculated
  # this relationship is from Hansen et al 2011 NAJFM, Escanaba study
  # Tsehaye et al 2016 gives the specific formula used for <age 5 walleye. Estimated lake-specific M for older walleye
  # I wonder if I can access that..
  NmortAgeYear <-lapply(exploitation, function(x) (0.7-0.92*x)*M)
  # Put floor on natural mortality, otherwise very high exploitation can result in negative M
  NmortAgeYear<-lapply(NmortAgeYear, function(x) {x[x<0]<-0.01; x})
  
  # now replace the appropriate columns (y) in NmortAge list
  # cool! now go back and do that for fishing.mortality
  wbics<-names(NmortAgeYear)
  
  if(burnin==F){
  NmortAge<-lapply(seq_along(NmortAge), function(x) {
    lake_matrix<-NmortAge[[x]]
    lake_name<-names(NmortAgeYear)[x]
    lake_vector<-NmortAgeYear[[lake_name]]
    
    lake_matrix[,y]<-lake_vector
    return(lake_matrix)

  })} else{
      NmortAge<-NmortAgeYear
    }
  
  names(NmortAge)<-wbics
  
  # still in current year--adjusting fishPops numbers by subtracting natural mortality. 
  # important: natural mortality acts on *starting* population, not population after harvest. 
  # this messed me up before.
  
  # a is fishPops, b is NmortAge, c is startPops. only fishPops includes year 0
  
  if(burnin==FALSE){
  fishPopsYear<-mapply(function(a,b,c) a[,y]-(b[,y]*c[,y]),
               a=fishPops,
               b=NmortAge,
               c=startPops)
  } else{
    fishPopsYear<-mapply(function(a,b,c) a[,y]-(b*c[,y]),
                         a=fishPops,
                         b=NmortAge,
                         c=startPops)
    
  }
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