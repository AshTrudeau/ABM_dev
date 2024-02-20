# first round of comparing the bare bones SES model with actual observed patterns. 
# comparing effort and harvest (as proportions) between trend lakes (with  many
# years of observations) to the same lakes as simulated
library(tidyverse)
library(here)
library(RColorBrewer)
library(wdnr.fmdb)


sim.annual<-read_csv(here::here("output","annual.output.csv"))
sim.annual[,1]<-NULL

# get lake characteristics from fmdb


trend.char<-get_fmdb_lakechar(wbic=unique(sim.annual$WBIC))

trend.char<-trend.char%>%
  filter(wbic%in%unique(sim.annual$WBIC))%>%
  select(wbic, waterbody.name, lake.area, wae.code)
 # mutate(wbic=factor(wbic, levels=wbic[order(lake.area)], ordered=T))


sim.annual<-sim.annual%>%
  mutate(wbic=WBIC)%>%
  left_join(trend.char, by="wbic")%>%
  mutate(wbic=factor(wbic, levels=unique(wbic[order(lake.area)]), ordered=T))

obs.effort<-read_csv(here::here("pattern_matching_data","annual.effort.trend.lakes.csv"))
obs.effort[,1]<-NULL
prop.effort<-read_csv(here::here("pattern_matching_data","mean.prop.fishing.effort.trend.csv"))


ggplot(sim.annual)+
  geom_line(aes(x=year, y=annualEffort, color=wbic))+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  theme_bw()


sim.annual.mean<-sim.annual%>%
  group_by(wbic)%>%
  summarize(mean.effort=mean(annualEffort), 
            sd.effort=sd(annualEffort),
            lake.area=unique(lake.area))%>%
  ungroup()%>%
  mutate(high.ci=mean.effort+2*sd.effort,
         low.ci=mean.effort-2*sd.effort)

ggplot(sim.annual.mean)+
  geom_pointrange(aes(x=wbic, y=mean.effort, ymin=low.ci, ymax=high.ci, color=wbic))+
  scale_color_manual(values=c(brewer.pal(n=12, "Paired"), "#000000"))+
  theme_bw()
