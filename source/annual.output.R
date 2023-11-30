annual.output<-function(y, fishery, annualOutput, parameters){
  lakeStatus<-fishery[["lakeStatus"]]
  lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  startPops<-fishery[["startPops"]]
  fishPops<-fishery[["fishPops"]]
  fishSizes<-fishery[["fishSizes"]]
  harvestAge<-fishery[["harvestAge"]]
  
  qualitySize<-parameters[["qualitySize"]]
  nYears<-parameters[["nYears"]]
  
  # don't use lakeStatus for end of year fish population! It hasn't been updated with natural mortality yet!
  
  # i'll need to set this up for multiple lakes. Take in list of fishPop matrices, put out vector of end of year
  # fish populations
  
  fishNStart<-lapply(startPops, function(x) sum(x[,y]))
  fishNEnd<-lapply(fishPops, function(x) sum(x[,y+1]))
  
  biomass.fun<-function(pop, weight){
    pop*weight
  }
  
  startPopsYear<-lapply(startPops, function(x) x[,y])
  endPopsYear<-lapply(fishPops, function(x) x[,y+1])
  
  # if growth responds to population density at some point, implement that here
  weightAge<-lapply(fishSizes, function(x) x$weight)
  
  fishBStart<-mapply(biomass.fun, startPopsYear, weightAge)
  fishBStart<-as.list(colSums(fishBStart))
  
  fishBEnd<-mapply(biomass.fun, endPopsYear, weightAge)
  fishBEnd<-as.list(colSums(fishBEnd))
  
  harvestAgeN<-sapply(harvestAge, function(x) x[,y])
  harvestAgeN<-as.list(as.data.frame(harvestAgeN))
  harvestAgeB<-mapply(biomass.fun, harvestAgeN, weightAge)
  harvestAgeB<-as.list(as.data.frame(harvestAgeB))
  
  annualHarvestN<-lapply(harvestAgeN, sum)
  annualHarvestB<-lapply(harvestAgeB, sum)
  


  # max size of fish in the lake by the end of year y after recruitment
  if(y==nYears){
    # on the last year of the simulation, no 'future' column exists, so take the last existing one
    allFish<-lapply(fishPops, function(x) x[,y+1])
  }else{
  
    allFish<-lapply(fishPops, function(x) x[,y+2])
  }
  # find maximum age
  max.age.fun<-function(allFish){
    # add logic for lakes with 0 fish population
    if(sum(allFish)!=0){
    which.max(as.numeric(names(allFish[which(allFish!=0)])))-1
    } else{
      0
    }
  }
  
  # numeric(0) values come out when the is a fish population of 0
  maxAge<-lapply(allFish, max.age.fun)
  

  # 
  
  max.length.fun<-function(fishSizes, maxAge){
    fishSizes[fishSizes$age==maxAge,]$length
  }
  
  maxLength<-mapply(max.length.fun, fishSizes, maxAge)
  
   # mean size of fish in the lake at the end of year y, after recruitment
  
  # this is where I left off before lunch
  
  mean.length<-function(allFish, fishSizes){
    if(sum(allFish)!=0){
    sum(allFish*fishSizes$length)/sum(allFish)
    } else{
      0
    }
  }
  
  meanLength<-mapply(mean.length, allFish, fishSizes)
  
  psd.fun<-function(allFish, fishSizes){
    if(sum(allFish)!=0){
    quality.ind<-ifelse(fishSizes$length>=qualitySize, 1, 0)
    total.fish<-sum(allFish)
    psd.calc<-sum(allFish[quality.ind==1])/total.fish
    return(psd.calc)
    } else{
      0
    }
  }
  
  psd<-mapply(psd.fun, allFish, fishSizes)
  
  psd<-as.list(psd)

  annualEffort<-lakeStatus%>%
    dplyr::filter(year==y)%>%
    dplyr::mutate(WBIC=factor(WBIC, levels=lakeCharacteristics$WBIC))%>%
    dplyr::group_by(WBIC)%>%
    dplyr::summarize(annualEffort=sum(nAnglers))%>%
    dplyr::ungroup()
  
  annualEffort<-as.list(annualEffort$annualEffort)
  
  annualOutputYear<-data.frame(lakeID=names(fishNStart),
                             year=rep(y),
                             fishNStart=unlist(fishNStart),
                             fishBStart=unlist(fishBStart),
                             fishNEnd=unlist(fishNEnd),
                             fishBEnd=unlist(fishBEnd),
                             annualHarvestN=unlist(annualHarvestN),
                             annualHarvestB=unlist(annualHarvestB),
                             annualEffort=unlist(annualEffort),
                             maxSize=unlist(maxLength),
                             meanSize=unlist(meanLength),
                             PSDQuality=unlist(psd)
                             )

  rownames(annualOutputYear)<-NULL
  
  annualOutput[annualOutput$year==y,]<-annualOutputYear
  
  return(annualOutput)
  

}