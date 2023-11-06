initialize.selectivity<-function(parameters){
  ageVulnerable<-parameters[["ageVulnerable"]]
  allAges<-parameters[["allAges"]]
  

  selectivity<-rep(0, length(allAges))
  agesVulnerable<-subset(allAges, allAges>=ageVulnerable)
  # account for age 0 by adding 1
  selectivity[agesVulnerable+1]<-1
  
return(selectivity)

}