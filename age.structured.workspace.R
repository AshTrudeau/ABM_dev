# Workspace to figure out age structured model

# Pull from DNR database to see what we're working with
library(halk)
library(wdnr.fmdb)
library(tidyverse)
library(lubridate)

# for manageability, let's start by pulling 'real' lakes from Vilas county. Given a small number
# of (stratified) randomly sampled lakes, how can I develop an age and size structured population
# model using existing data?

# drop 'simple' lakes (for now), don't have walleye
# at some point I'll have to add indicator for bluegill, lmb, walleye presence, make those population models. 
# in multispecies version I'll drop only No Fishery lakes and Trout Ponds

lakeClasses<-read_csv(here::here("data","all.lake.classes.wi.csv"))

# pull lake classes for Vilas county
classes.vilas<-lakeClasses%>%
  filter(County=="Vilas" & grepl("Complex", `Final Lake Class`))%>%
  group_by(`Final Lake Class`)%>%
  summarise(n=n())%>%
  ungroup()%>%
  mutate(total=sum(n),
         prop=n/total)%>%
  rename("lakeClass"=`Final Lake Class`)%>%
  mutate(outOf15=round(15*prop))

# add probability of selection to df of Vilas lakes, then select random sample

n.samples<-classes.vilas$outOf15

# helpful tutorial https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html 

vilas.sample<-lakeClasses%>%
  filter(County=="Vilas" & grepl("Complex", `Final Lake Class`))%>%
  rename("lakeClass"=`Final Lake Class`)%>%
  group_by(lakeClass)%>%
  nest()%>%
  ungroup()%>%
  mutate(n=n.samples)%>%
  mutate(sample=map2(data, n, sample_n))%>%
  select(!data)%>%
  unnest(sample)
  
# now find DNR data for these lakes

sampled.wbic<-vilas.sample$WBIC

# that actually just gave me all the lakes in vilas county
effort.sample<-get_fmdb_efforts(wbic=sampled.wbic)

surveys.sample<-get_fmdb_surveys(wbic=sampled.wbic)
  
surveys.type<-surveys.sample%>%
  mutate(year=year(ymd(survey.begin.date)))%>%
  group_by(wbic, primary.survey.purpose)%>%
  summarize(nYears=length(unique(year)),
            lakeName=unique(waterbody.name),
            minYear=min(year),
            maxYear=max(year))
  

# of the vilas county lakes classified, how many have been surveyed by dnr? 
effort<-get_fmdb_efforts(county="vilas", year=c(2010:2023))


creel_counts<-get_creel_counts(county="vilas", year=c(2010:2023))

# Create an age 0 population for each lake. 

