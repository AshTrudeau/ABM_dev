annual.output<-function(y, fishery, annualOutput, parameters){
  lakeStatus<-fishery[["lakeStatus"]]
  startPop<-fishery[["startPop"]]
  fishPop<-fishery[["fishPop"]]
  fishSize<-fishery[["fishSize"]]
  harvestAge<-fishery[["harvestAge"]]
  
  qualitySize<-parameters[["qualitySize"]]
  nYears<-parameters[["nYears"]]
  
  # don't use lakeStatus for end of year fish population! It hasn't been updated with natural mortality yet!
  
  # i'll need to set this up for multiple lakes. Take in list of fishPop matrices, put out vector of end of year
  # fish populations
  fishNStart<-sum(startPop[,y])
  fishBStart<-sum(startPop[,y]*fishSize$weight)
  
  fishNEnd<-sum(fishPop[,y+1])
  fishBEnd<-sum(fishPop[,y+1]*fishSize$weight)
  
  annualHarvestN<-sum(harvestAge[,y])
  annualHarvestB<-sum(harvestAge[,y]*fishSize$weight)
  
  # max size of fish in the lake by the end of year y after recruitment
  if(y==nYears){
    # on the last year of the simulation, no 'future' column exists, so take the last existing one
    allFish<-fishPop[,y+1]
  }else{
  
  allFish<-fishPop[,y+2]
  }
  # minus 1 because age matrix starts at age 0
  maxAge<-which.max(as.numeric(names(allFish[which(allFish!=0)])))-1
  maxLength<-fishSize[fishSize$age==maxAge,]$length
 
   # mean size of fish in the lake at the end of year y, after recruitment
  
  meanLength<-sum((allFish*fishSize$length))/sum(allFish)
  
  psd<-data.frame(nFish=allFish, length=fishSize$length)
  psd$quality<-ifelse(psd$length>=qualitySize, 1, 0)
  
  psd.calc<-sum(psd[psd$quality==1,]$nFish)/sum(psd$nFish)
  
  lakeYear<-lakeStatus%>%
    filter(year==y)%>%
    group_by(lakeID)%>%
    summarize(annualEffort=sum(nAnglers))
  

  # effort is a placeholder; need to set up multiple lakes
  lakeStatusYear<-data.frame(lakeID=lakeYear$lakeID,
                              year=y,
                             fishNStart=fishNStart, 
                             fishBStart=fishBStart,
                             fishNEnd=fishNEnd,
                             fishBEnd=fishBEnd,
                             annualHarvestN=annualHarvestN,
                             annualHarvestB=annualHarvestB,
                             annualEffort=lakeYear$annualEffort,
                             maxLength=maxLength,
                             meanLength=meanLength,
                             PSDQuality=psd.calc
                             )
  
  
 
  
  annualOutput[annualOutput$year==y,]<-lakeStatusYear
  
  return(annualOutput)
  

}