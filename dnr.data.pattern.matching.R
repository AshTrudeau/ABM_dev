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


# for now I am using trend lakes because they have the most data available. Later, 
# once the simulation adopts more spatial coverage, I can use GLMMs to produce 
# fishing effort estimates for many more lakes
library(wdnr.fmdb)
library(tidyverse)
library(here)
library(lubridate)
library(RColorBrewer)
library(cowplot)

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
write_rds(trend.wbics, "trend.wbics.Rds")

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
         annual.sd=sqrt(sum(total.effort.sd^2)),
         nSamples=sum(n.counts))%>%
  mutate(nYears=length(unique(year)))%>%
  ungroup()%>%
  mutate(wbic=as.factor(wbic))

# I was overthinking this. DNR calcs just use sum of variance. (sqrt sum sd^2)

# https://stats.stackexchange.com/questions/25848/how-to-sum-a-standard-deviation/442050#442050
# mean.sd.effort<-annual.trend.effort%>%
#   group_by(wbic)%>%
#   mutate(mean.annual.effort=sum(annual.effort*nSamples)/sum(nSamples),
#          totalSamples=sum(nSamples))%>%
#   ungroup()%>%
#   mutate(top=annual.sd^2*(nYears-1)+nYears*(mean.annual.effort-annual.effort)^2,
#          bottom=totalSamples-1)%>%
#   group_by(wbic)%>%
#   summarize(sd.annual.effort=sqrt(sum(top)/unique(bottom)),
#             mean.annual.effort=unique(mean.annual.effort),
#             sd.between.years=sd(annual.effort))%>%
#   mutate(overall.sd=sqrt(sd.annual.effort^2+sd.between.years^2))%>%
#   select(wbic, mean.annual.effort, overall.sd)%>%
#   mutate(upper=mean.annual.effort+2*overall.sd,
#          lower=mean.annual.effort-2*overall.sd)

# I'm not going to overthink it. just do straight mean of estimates, then sqrt sum of
# variance of within and between estimates

 mean.sd.effort<-annual.trend.effort%>%
   group_by(wbic)%>%
   mutate(mean.annual.effort=mean(annual.effort),
          totalSamples=sum(nSamples))%>%
   summarize(sd.annual.effort.between=sd(annual.effort),
             sd.annual.effort.within=sqrt(sum(annual.sd^2)),
             mean.annual.effort=unique(mean.annual.effort))%>%
   ungroup()%>%
   mutate(sd.annual.effort=sqrt(sd.annual.effort.between^2+sd.annual.effort.within^2))%>%
   select(wbic, mean.annual.effort, sd.annual.effort)%>%
   mutate(upper.ci=mean.annual.effort+2*sd.annual.effort,
          lower.ci=mean.annual.effort-2*sd.annual.effort)
    
     
    

# look at effort with sd for each lake across years

ggplot()+
  geom_point(data=annual.trend.effort, aes(x=wbic, y=annual.effort), color="darkgray", size=2)+
  geom_pointrange(data=mean.sd.effort, aes(x=wbic,y=mean.annual.effort, 
                       ymin=lower.ci, 
                       ymax=upper.ci, group=wbic, color=wbic),
                  linewidth=1,
                  size=0.75)+
  scale_color_manual(values=c(brewer.pal(n=12,  "Paired"), "#000000"))+
  xlab("WBIC")+
  ylab("Annual fishing effort estimates")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        legend.position="none")
ggsave(here::here("figures","trend.lake.mean.fishing.effort.png"), height=4, width=6)
  
write.csv(mean.sd.effort,  here::here("pattern_matching_data","mean.fishing.effort.trend.lakes.consider.n.csv"))
write.csv(annual.trend.effort, here::here("pattern_matching_data","annual.effort.trend.lakes.csv"))

# can I get lake surface area? 
trend.char<-get_fmdb_lakechar(wbic=trend.wbics)%>%
  # weird, it gave me all of them?
  filter(wbic%in%trend.wbics)%>%
  mutate(wbic=as.factor(wbic))
#  aha, lake area and walleye code (nr, cst, cnr, st)

mean.sd.effort.char<-left_join(mean.sd.effort, 
                               trend.char[,c("wae.code", "max.depth","lake.area","wbic")], by="wbic")%>%
    mutate(wbic=factor(wbic, levels=wbic[order(lake.area)], ordered=T))

ggplot(mean.sd.effort.char)+
  geom_pointrange(aes(x=wbic, y=mean.annual.effort, ymin=lower.ci, ymax=upper.ci, color=wbic))+
  scale_color_manual(values=palette.reord)+
  ylab("Mean annual effort")+
  xlab("WBIC")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))
ggsave(here::here("figures","trend.lake.mean.fishing.effort.size.order.png"), height=4, width=6)
# some lake have quite a lot of variance, others not so much

palette.org<-c(brewer.pal(12, "Paired"), "#000000")
palette.reord<-c("#FFFF99", "#1F78B4", "#000000", "#B15928", 
                 "#CAB2D6","#33A02C", "#6A3D9A", "#B2DF8A",
                 "#FB9A99", "#A6CEE3", "#FF7F00", "#FDBF6F",
                 "#E31A1C" )

# manually reordering color palette


# now compare to simulated f ishing effort

sim.effort<-read_csv(here::here("output","annual.output.csv"))%>%
  group_by(WBIC)%>%
  summarize(mean.annual.effort=mean(annualEffort),
            sd.annual.effort=sd(annualEffort))%>%
  ungroup()%>%
  mutate(wbic=as.factor(WBIC))%>%
  left_join(trend.char[,c("wbic","lake.area")])%>%
  mutate(wbic=factor(wbic, levels=unique(wbic[order(lake.area)]), ordered=T))%>%
  mutate(ci.upper=mean.annual.effort+2*sd.annual.effort,
         ci.lower=mean.annual.effort-2*sd.annual.effort)

ggplot(sim.effort)+
  geom_pointrange(aes(x=wbic, y=mean.annual.effort, ymin=ci.lower, ymax=ci.upper, color=wbic))+
  scale_color_manual(values=palette.reord)+
  ylab("Mean annual effort")+
  xlab("WBIC")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))
ggsave(here::here("figures","trend.lake.sim.fishing.effort.png"), height=4, width=6)

creel_int<-get_creel_int_party(wbic=trend.wbics)
# Next get rec harvest  and walleye PSD. No obvious way to get population estimates yet--just use raw data?
# 


creel_fish<-get_creel_fish_data(wbic=trend.wbics)
creel_harv_rate<-calc_creel_harvest_rates(creel_fish)
creel_harv_est<-estimate_harvest(trend.creel.counts, creel_int, creel_fish)
# this version has species-specific catch rates for different months, which is useful
write.csv(creel_harv_est, here::here("pattern_matching_data","harvest.estimates.trend.lakes.all.species.csv"))


# this is where I left off fixing SDs 1/18
# aha, this includes species-specific effort estimates!
annual.walleye.harvest<-creel_harv_est%>%
  filter(species=="walleye")%>%
  group_by(wbic, year)%>%
  summarize(waterbody.name=unique(waterbody.name),
            annual_catch=sum(spp.catch),
            annual_harvest=sum(spp.harvest),
            annual_spp_effort=sum(total.spp.effort),
            #  now get sd
            catch_sd=sqrt(sum(catch.var)),
            harvest_sd=sqrt(sum(harvest.var)),
            spp.effort=sum(total.spp.effort))%>%
  mutate(nYears=length(unique(year)))%>%
  ungroup()

write.csv(annual.walleye.harvest, here::here("pattern_matching_data","annual.walleye.harvest.catch.trend.csv"))

lake.mean.harvest<-annual.walleye.harvest%>%
  group_by(wbic)%>%
  summarize(mean.harvest=mean(annual_harvest),
            mean.catch=mean(annual_catch),
            harvest.sd.across=sd(annual_harvest),
            catch.sd.across=sd(annual_catch),
            harvest.sd.within=sqrt(sum((harvest_sd)^2)),
            catch.sd.within=sqrt(sum((catch_sd)^2)),
            mean.spp.effort=mean(spp.effort),
            sd.spp.effort=sd(spp.effort))%>%
  ungroup()%>%
  mutate(harvest.sd=sqrt(harvest.sd.across^2+harvest.sd.within^2),
         catch.sd=sqrt(catch.sd.across^2+catch.sd.within^2),
         harvest.upper.ci=mean.harvest+2*harvest.sd,
         harvest.lower.ci=mean.harvest-2*harvest.sd,
         catch.upper.ci=mean.catch+2*catch.sd,
         catch.lower.ci=mean.catch-2*catch.sd,
         wbic=as.factor(wbic),
         upper.spp.effort=mean.spp.effort+2*sd.spp.effort,
         lower.spp.effort=mean.spp.effort-2*sd.spp.effort)%>%
  select(wbic, mean.harvest, mean.catch, catch.sd, harvest.sd, harvest.upper.ci, 
         harvest.lower.ci, catch.upper.ci, catch.lower.ci)%>%
  left_join(trend.char[,c("wbic","lake.area","wae.code")], by="wbic")%>%
mutate(wbic=factor(wbic, levels=wbic[order(lake.area)], ordered=T))

write.csv(lake.mean.harvest, here::here("pattern_matching_data","mean.catch.harvest.trend.csv"))

# mean harvest
harvest<-ggplot(lake.mean.harvest)+
  geom_pointrange(aes(x=wbic, y=mean.harvest, ymin=harvest.upper.ci, 
                      ymax=harvest.lower.ci, color=wae.code))+
  scale_color_manual(values=brewer.pal(4, "Set1"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

legend<-get_legend(harvest)
harvest<-ggplot(lake.mean.harvest)+
  geom_pointrange(aes(x=wbic, y=mean.harvest, ymin=harvest.upper.ci, 
                      ymax=harvest.lower.ci, color=wae.code))+
  scale_color_manual(values=brewer.pal(4, "Set1"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")


# mean catch

catch<-ggplot(lake.mean.harvest)+
  geom_pointrange(aes(x=wbic, y=mean.catch, ymin=catch.upper.ci, 
                      ymax=catch.lower.ci, color=wae.code))+
  scale_color_manual(values=brewer.pal(4, "Set1"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")


effort<-ggplot(lake.mean.harvest)+
  geom_point(aes(x=wbic, y=mean.catch,  color=wae.code))+
  scale_color_manual(values=brewer.pal(4, "Set1"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")

plot_grid(effort, catch, harvest, legend, ncol=1)
ggsave(here::here("figures","wae.effort.catch.harvest.trend.png"), height=12, width=6)  


# and size/PSD (quality)

trend.fish<-get_fmdb_fishraw(wbic=trend.wbics)

trend.psd<-calc_psd(trend.fish)
  
trend.psd.char<-trend.psd%>%
  filter(species=="walleye")%>%
  mutate(wbic=as.factor(wbic))%>%
  left_join(trend.char[,c("lake.area","wae.code","wbic")])%>%
  mutate(wbic=factor(wbic, levels=unique(wbic[order(lake.area)]), ordered=T))
write.csv(trend.psd.char, here::here("pattern_matching_data","walleye.size.trend.csv"))

# huge variation within lakes. interesting that the NR lake PSDs are so small..
ggplot(trend.psd.char)+
  geom_point(aes(x=wbic, y=quality, color=wae.code))+
  theme_bw()

# ohh it's because those lakes have bigger walleye. duh.
ggplot(trend.psd.char)+
  geom_point(aes(x=wbic, y=memorable, color=wae.code))+
  theme_bw()

ggplot(trend.psd.char)+
  geom_point(aes(x=wbic, y=trophy, color=wae.code))+
  theme_bw()

#==============================================================
# now convert absolute values to proportions for comparison with simulation data

# fishing effort
# using annual estimates is a problem because not all lakes are surveyed every year. So a year where 
# where only 2 lakes were surveyed will have a greater proportion of effort on one of those lakes than 
# an 'identical' year where 4 lakes were surveyed.

# prop.annual.effort<-annual.trend.effort%>%
#   group_by(year)%>%
#   mutate(annual.total=sum(annual.effort),
#          nLakes=length(unique(wbic)))%>%
#   ungroup()%>%
#   mutate(prop.annual.effort=annual.effort/annual.total)%>%
#   left_join(trend.char[,c("wbic","wae.code","lake.area")], by="wbic")%>%
#   mutate(wbic=factor(wbic, levels=unique(wbic[order(lake.area)])), ordered=T)
# 

mean.prop.effort<-mean.sd.effort%>%
  mutate(total.mean=sum(mean.annual.effort),
         mean.prop=mean.annual.effort/total.mean,
         upper.mean.prop=mean.prop+2*(sd.annual.effort/total.mean),
         lower.mean.prop=mean.prop-2*(sd.annual.effort/total.mean))%>%
  left_join(trend.char[,c("wbic","lake.area","wae.code")], by="wbic")%>%
  mutate(wbic=factor(wbic, levels=wbic[order(lake.area)]), ordered=T)

write.csv(mean.prop.effort, here::here("pattern_matching_data","mean.prop.fishing.effort.trend.csv"))

ggplot(mean.prop.effort)+
  geom_pointrange(aes(x=wbic, y=mean.prop, ymin=lower.mean.prop, ymax=upper.mean.prop, color=wae.code))+
  scale_color_manual(values=brewer.pal(4, "Set1"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))

# what about walleye-specific effort? 




# now do harvest
mean.prop.harvest<-mean.sd.effort%>%
  mutate(total.mean=sum(mean.annual.effort),
         mean.prop=mean.annual.effort/total.mean,
         upper.mean.prop=mean.prop+2*(sd.annual.effort/total.mean),
         lower.mean.prop=mean.prop-2*(sd.annual.effort/total.mean))%>%
  left_join(trend.char[,c("wbic","lake.area","wae.code")], by="wbic")%>%
  mutate(wbic=factor(wbic, levels=wbic[order(lake.area)]), ordered=T)


