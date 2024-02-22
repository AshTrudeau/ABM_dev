plotting.lake.status<-function(annualOutput, lakeStatus, lakeCharacteristics, parameters){
  #  note: for now this script will only run for a maximum of 13 lakes
  nLakes<-parameters[["nLakes"]]
  
  # lakeStatus<-fishery[["lakeStatus"]]
  # lakeCharacteristics<-fishery[["lakeCharacteristics"]]
  # anglerCharacteristics<-fishery[["anglerCharacteristics"]]
  
  annualOutput$WBIC<-as.factor(annualOutput$WBIC)
  

  effort<-ggplot(annualOutput)+
    geom_line(aes(x=year, y=annualEffort, color=WBIC), linewidth=1.5)+
    #scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
    xlab("Year of simulation")+
    ylab("Fishing effort (angler visits per year)")+
    guides(color="none")+
    theme_bw()
  
  harvest<-ggplot(annualOutput)+
    geom_line(aes(x=year, y=annualHarvestN, color=WBIC), linewidth=1.5)+
   # scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
    xlab("Year of simulation")+
    ylab("Annual harvest (N)")+
    guides(color=guide_legend(title="WBIC"))+
    theme_bw()
  
  fishPop<-ggplot(annualOutput)+
    geom_line(aes(x=year, y=fishNEnd, color=WBIC), linewidth=1.5)+
    #scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
    xlab("Year of simulation")+
    ylab("End of year population (N)")+
    guides(color="none")+
    theme_bw()
  
  lakeOutput<-annualOutput%>%
    group_by(WBIC)%>%
    summarize(effort=sum(annualEffort))%>%
    ungroup()%>%
    mutate(WBIC=as.character(WBIC))%>%
    left_join(lakeCharacteristics, by="WBIC")%>%
    mutate(WBIC=factor(WBIC, levels=levels(annualOutput$WBIC)))
  
  
  hotspots<-ggplot()+
    geom_point(data=lakeOutput, aes(x=lakeLong, y=lakeLat, color=WBIC, size=effort))+
    geom_point(data=anglerCharacteristics, aes(x=anglerLong, y=anglerLat), shape=4)+
    #scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
    guides(color="none",
           size=guide_legend(title="Fishing\neffort"))+
    xlab("X coord")+
    ylab("Y coord")+
    theme_bw()
    
  
  plot_grid(effort, harvest, fishPop, hotspots)

}