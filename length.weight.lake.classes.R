# this script estimates length weight relationships for walleye, bluegill, and largemouth bass in 
# each of the lake classes described by Rypel et al 2019. 

library(tidyverse)
library(here)
library(broom)

# Length at age is provided by the DNR. Use w = alpha * length ^ beta to estimate weight at age
# for now, only walleye. later include bluegill and largemouth bass

lengthAge<-read_csv(here("data", "lake class standards", "Lake Class Standards Length Age.csv"))%>%
  filter(Species%in%c("Walleye"))

# For now, I'm converting length to age with the length-weight relationship (from WDNR website) weight.lb=(length^3)/2700


length.weight<-function(c, rho, lengths.mm){
  # convert lengths in mm to inches
  lengths.in=lengths.mm/25.4
  weights.lb=(lengths.in^3)/2700
  weights.kg=weights.lb*0.45
  return(weights.kg)
}

lengthAgeWeight<-lengthAge%>%
  rename("medianLength.mm"=`Median Total Length (mm)`)%>%
  mutate(medianWeightAge.t.kg=length.weight(c=-5.3596, rho=3.2162, lengths.mm=medianLength.mm))%>%
  select(Species, LakeClass, Age, medianLength.mm, medianWeightAge.t.kg)%>%
  group_by(Species, LakeClass)%>%
  mutate(medianWeightAge.t1.kg=lead(medianWeightAge.t.kg))%>%
  ungroup()

write.csv(lengthAgeWeight, here("data","walleye.lakeClass.length.weight.csv"))

lengthAgeWeight.lm<-lengthAgeWeight%>%
  filter(!is.na(medianWeightAge.t1.kg))%>%
  # use purrr to run repeated linear regressions on weight t1 vs t and store slope and intercept
  group_by(LakeClass)%>%
  nest()%>%
  # regress weight age + 1 vs weight at age to get slope and intercpet
  mutate(mod=map(data, ~lm(data=., medianWeightAge.t1.kg ~ medianWeightAge.t.kg)),
         tidied=map(mod, tidy))%>%
  unnest(tidied)%>%
  select(LakeClass, term, estimate)%>%
  pivot_wider(names_from = term,
              values_from=estimate)%>%
  rename("c"=`(Intercept)`,
         "rho"=medianWeightAge.t.kg)
  
write.csv(lengthAgeWeight.lm, here("data","walleye.lakeClass.length.weight.lm.csv"))



