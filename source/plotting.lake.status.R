plotting.lake.status<-function(output, fishery, parameters, lakeLocation, anglerCharacteristics){
  #  note: for now this script will only run for a maximum of 12 lakes
  nLakes=parameters[["nLakes"]]
  
  lakeStatus<-output[["lakeStatus"]]
  
  
  # aggregate effort and harvest by year
  annualOutcomes<-lakeStatus%>%
    group_by(year, lakeID)%>%
    summarize(effort=sum(nAnglers),
              harvest=sum(nHarvested),
              finalPop=fishPop[which.max(day)])%>%
    ungroup()%>%
    filter(year>0)
    
  # aggregate by lake and join to locations for distribution plot
  
  lakeOutcomes<-annualOutcomes%>%
    group_by(lakeID)%>%
    summarize(effort=sum(effort))%>%
    left_join(lakeLocation, by="lakeID")
  

  effort<-ggplot(annualOutcomes)+
    geom_line(aes(x=year, y=effort, color=as.factor(lakeID)), linewidth=1.5)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    xlab("Year of simulation")+
    ylab("Fishing effort (angler visits per year)")+
    guides(color=guide_legend(title="LakeID"))+
    theme_bw()
  
  harvest<-ggplot(annualOutcomes)+
    geom_line(aes(x=year, y=harvest, color=as.factor(lakeID)), linewidth=1.5)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    xlab("Year of simulation")+
    ylab("Annual harvest (N)")+
    guides(color=guide_legend(title="LakeID"))+
    theme_bw()
  
  fishPop<-ggplot(annualOutcomes)+
    geom_line(aes(x=year, y=finalPop, color=as.factor(lakeID)), linewidth=1.5)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    xlab("Year of simulation")+
    ylab("End of year population (N)")+
    guides(color=guide_legend(title="LakeID"))+
    theme_bw()
  
  hotspots<-ggplot()+
    geom_point(data=lakeOutcomes, aes(x=x, y=y, color=as.factor(lakeID), size=effort))+
    geom_point(data=anglerCharacteristics, aes(x=x.a, y=y.a), shape=4)+
    scale_color_manual(values=brewer.pal(n=nLakes, "Paired"))+
    guides(color="none",
           size=guide_legend(title="Fishing\neffort"))+
    xlab("X coord")+
    ylab("Y coord")+
    theme_bw()
    
  
  plot_grid(effort, harvest, fishPop, hotspots)
}