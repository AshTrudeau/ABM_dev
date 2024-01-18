# This script pulls data from the WDNR FMDB for trend lakes--lakes with multiple years
# of survey data. I'm going to start comparing patterns in fishing effort, walleye populations, 
# walleye population density, and catch rates between the simulation model and reality.

# Note: THIS SCRIPT REQUIRES WDNR FMDB ACCESS

# some unresolved questions: 
# 1. accounting for differences in lake productivity. For now, the only 
# differences between lakes are lake surface area and VBGF parameters by Rypel et al 2019 lake 
# class. Differences in harvest then feed into differences in  natural mortality. Recruitment
# is deterministic with identical alpha and beta  parameters for each lake (regional estimates from
# Tsehaye 2016)
#  2. Handling NR, combination, and stocked lakes. stick with NR for this pattern matching? I will
# eventually need to include stochastic recruitment, lake-specific differences in recruitment, 
# and stocking. newer mixed model approach to predicting unsampled walleye populations may be3 
# important here (G. Hansen et al 2015, NAJFM)
# 3. P, B, and P/B will be important pattern matching metrics to include. From Rypel et al 2018 (CJFAS)
# Production can be calculated with the instantaneous growth method: production (kg/ha/year)
# P = sum of (year's insstantaneous growth for age class i * mean standing stock biomass (kg/ha)during the year for age class i)
# instantaneous growth for the year = ln(mean mass time t/mean mass time t+1)
# 4. What size structure metrics can I compare? (see what's available in FMDB)

library(wdnr.fmdb)
library(tidyverse)
library(here)
library(lubridate)
library(RColorBrewer)

trend.surveys<-get_fmdb_surveys(waterbody_name=c("Balsam Lake","Big Arbor Vitae Lake",
                                                 "Bony Lake","Diamond Lake",
                                                "Grindstone Lake","Metonga Lake", 
                                                "Lipsett Lake","Middle Eau Claire Lake",
                                                "Pine Lake","Plum Lake","Snipe Lake",
                                                "Trout Lake","Two Sisters Lake"
                                       ))
# this has warning messages because of  multiple lakes w ith the same name

trend<-read_csv(here::here("data","trend.lakes.csv"))%>%
  filter(agency=="WDNR")%>%
  mutate(county=tolower(county),
         lakeName=tolower(lakeName),
         lakeName=str_replace_all(lakeName, " ", "_"),
         lakeName_county=paste(lakeName, county, sep="_"))

trend.surveys<-trend.surveys%>%
  mutate(lakeName_county=paste(waterbody.name, county, sep="_"))

trend.surveys.f<-filter(trend.surveys, lakeName_county%in%trend$lakeName_county)

`%!in%`<-Negate(`%in%`)
filter(trend, lakeName_county%!in%trend.surveys.f$lakeName_county)
# missing Lake Metonga

metonga.surveys<-get_fmdb_surveys(wbic=394400)
trend.surveys.f$lakeName_county<-NULL

trend.surveys<-rbind.data.frame(trend.surveys.f, metonga.surveys)

trend.wbics<-unique(trend.surveys$wbic)

# but I actually want years too..

trend.surveys.creel<-trend.surveys%>%
  filter(grepl("creel",primary.survey.purpose) | grepl("creel", secondary.survey.purpose))%>%
  mutate(survey.begin.date=ymd(survey.begin.date),
         survey.end.date=ymd(survey.end.date))%>%
  group_by(wbic, year)%>%
  summarize(across(.cols=c(waterbody.name, county, latitude, longitude), unique))%>%
  ungroup()

# get effort and harvest estimates for  trend lakes

trend.creel.counts<-get_creel_counts(wbic=trend.wbics)
trend.effort<-calc_creel_effort(trend.creel.counts)
sum.trend.effort<-sum_monthly_creel_effort(trend.creel.counts)

annual.trend.effort<-sum.trend.effort%>%
  group_by(wbic, year)%>%
  summarize(annual.effort=sum(total.effort),
         annual.sd=sqrt(sum(total.effort.sd^2)))%>%
  mutate(nYears=length(unique(year)))%>%
  ungroup()%>%
  mutate(wbic=as.factor(wbic))

# i'm not sure I did this right
mean.sd.effort<-annual.trend.effort%>%
  group_by(wbic)%>%
  summarize(mean.annual.effort=mean(annual.effort),
            sd.annual.effort=sd(annual.effort),
         sd.sum.effort=sqrt(sum(annual.sd^2)),
         sd.mean.effort=sqrt((sd.sum.effort/unique(nYears))^2),
         overall.sd=sqrt(sd.mean.effort^2+sd.annual.effort^2))%>%
  select(wbic, mean.annual.effort, overall.sd)%>%
  mutate(upper=mean.annual.effort+2*overall.sd,
         lower=mean.annual.effort-2*overall.sd)

# look at effort with sd for each lake across years

ggplot()+
  geom_point(data=annual.trend.effort, aes(x=wbic, y=annual.effort), color="darkgray", size=2)+
  geom_pointrange(data=mean.sd.effort, aes(x=wbic,y=mean.annual.effort, 
                       ymin=lower, 
                       ymax=upper, group=wbic, color=wbic),
                  linewidth=1,
                  size=0.75)+
  scale_color_manual(values=c(brewer.pal(n=12,  "Paired"), "#000000"))+
  xlab("WBIC")+
  ylab("Annual fishing effort estimates")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        legend.position="none")
ggsave(here::here("figures","trend.lake.mean.fishing.effort.png"), height=4, width=6)
  
write.csv(mean.sd.effort,  here::here("pattern_matching_data","mean.fishing.effort.trend.lakes.csv"))
write.csv(annual.trend.effort, here::here("pattern_matching_data","annual.effort.trend.lakes.csv"))

# some lake have quite a lot of variance, others not so much

# Next get population estimates for walleye
