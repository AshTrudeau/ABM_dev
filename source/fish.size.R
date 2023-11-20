fish.size<-function(parameters, vbgf){
  
  # eventually fishSize will be a nested list with one object per waterbody; meant to match fishPop object
  allAges<-parameters[["allAges"]]
  
  linf<-vbgf$linf
  k<-vbgf$k
  t0<-vbgf$t0
  
  # note that length is in centimeters and weight in kilograms, but DNR conversion assumes inches and pounds. need to convert
  fishSize<-data.frame(age=allAges,
                       length=rep(NA),
                       weight=rep(NA))
  
  # for now making the bonkers assumption that fish don't grow after age 15
  
  # weight conversion from here https://dnr.wisconsin.gov/topic/Fishing/questions/estfishweight.html 
  
  fishSize$length<-linf*(1-exp(-k*(fishSize$age-t0)))
  fishSize$weight<-(((fishSize$length/2.54)^3)/2700)*0.454
  return(fishSize)
  
}