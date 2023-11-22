allLakes<-read_csv(here::here("data","all.lake.classes.wi.csv"))

# temporary code: selecting by proportion among complex classes

propComplex<-allLakes%>%
  filter(grepl("Complex", `Final Lake Class`))%>%
  mutate(nLakes=length(unique(WBIC)))%>%
  group_by(`Final Lake Class`)%>%
  summarize(n=n(),
            nLakes=unique(nLakes))%>%
  ungroup()%>%
  mutate(propClass=n/nLakes)

write.csv(propComplex, "complex.lake.proportion.csv")
