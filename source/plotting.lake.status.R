plotting.lake.status<-function(annualOutput, fishery, parameters){
  #  note: for now this script will only run for a maximum of 12 lakes
  nLakes<-parameters[["nLakes"]]
  
  lakeStatus<-fishery[["lakeStatus"]]
  lakeLocation<-fishery[["lakeLocation"]]
  anglerCharacteristics<-fishery[["anglerCharacteristics"]]
  
  
  lakeOutput<-annualOutput%>%
    group_by(lakeID)%>%
    summarize(effort=sum(annualEffort))%>%
    left_join(lakeLocation, by="lakeID")
  

  effort<-ggplot(annualOutput)+
    geom_line(aes(x=year, y=annualEffort, color=as.factor(lakeID)), linewidth=1.5)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    xlab("Year of simulation")+
    ylab("Fishing effort (angler visits per year)")+
    guides(color="none")+
    theme_bw()
  
  harvest<-ggplot(annualOutput)+
    geom_line(aes(x=year, y=annualHarvestN, color=as.factor(lakeID)), linewidth=1.5)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    xlab("Year of simulation")+
    ylab("Annual harvest (N)")+
    guides(color=guide_legend(title="LakeID"))+
    theme_bw()
  
  fishPop<-ggplot(annualOutput)+
    geom_line(aes(x=year, y=fishNEnd, color=as.factor(lakeID)), linewidth=1.5)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    xlab("Year of simulation")+
    ylab("End of year population (N)")+
    guides(color="none")+
    theme_bw()
  
  hotspots<-ggplot()+
    geom_point(data=lakeOutput, aes(x=x, y=y, color=as.factor(lakeID), size=effort))+
    geom_point(data=anglerCharacteristics, aes(x=x.a, y=y.a), shape=4)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    guides(color="none",
           size=guide_legend(title="Fishing\neffort"))+
    xlab("X coord")+
    ylab("Y coord")+
    theme_bw()
    
  
  plot_grid(effort, harvest, fishPop, hotspots)
}