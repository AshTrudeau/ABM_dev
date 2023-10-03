# 10/2/2023
# Beginning v.1 of recreational fishing ABM
# 10 lakes with non-reproducing fish populations. Lakes are randomly placed on a grid.
# 50 'anglers' with a randomly selected home on the grid. Straight line distance is calculated to each lake.
# Anglers choose the closest lake, harvest fish from there, and then go home.
# this repeats 100x

library(tidyverse)

set.seed(8329)

# grid is 50x50

# randomly selected points for lakes
x<-runif(10, min=1, max=50)
y<-runif(10, min=1, max=50)
lakeID<-seq(1:10)

lakeLoc<-cbind.data.frame(lakeID, x, y)
plot(y~x, lakeLoc)

# randomly selected home locations for anglers
x.a<-runif(50, min=1, max=50)
y.a<-runif(50, min=1, max=50)
anglerID<-seq(1:50)

anglerLoc<-cbind.data.frame(anglerID, x.a, y.a)

plot(y.a~x.a, anglerLoc)


# calculate Euclidean distance between each angler and each lake

distance<-function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2+(y1 - y2)^2))
} 

# use expand.grid to get all combinations of anglerID and lakeID, then left join coordinates and estimate distances
all.comb<-expand_grid(anglerLoc$anglerID, lakeLoc$lakeID)%>%
  rename("anglerID"=`anglerLoc$anglerID`,
         "lakeID"=`lakeLoc$lakeID`)%>%
  left_join(anglerLoc, by="anglerID")%>%
  left_join(lakeLoc, by="lakeID")%>%
  mutate(distance=distance(x.a, y.a, x, y),
         anglerID=as.character(anglerID),
         lakeID=as.character(lakeID))

# pivoting wider did something weird. I'll keep it long for now. 

lakeFish.0<-data.frame(lakeID=seq(1:10),
                     catch.param=sample(c(1,2,3,4), size=max(lakeID), replace=T ),
                     nFish=rep(100),
                     t=rep(0),
                     harvest=rep(0))

catchRecord<-lakeFish.0

anglers<-data.frame(anglerID=seq(1:50),
                    lakeChoice=rep(NA),
                    t=rep(NA),
                    harvest=rep(NA))

# start initial funciton-writing. Simulation will repeat over 30 time steps for now. Next steps will be making these functions modular. 

# lake choice--relies on all.comb df

for(i in 1:max(anglers$anglerID)){
  
  indiv<-all.comb[all.comb$anglerID==i,]
  # decision rule
  anglers[i,"lakeChoice"]<-indiv[which.min(indiv$distance),]$lakeID
}



# harvest--just a poisson draw. but let's have different parameters by lake. I need that link to lakeIDs for future iterations

for(i in 1:max(anglers$anglerID)){
  anglers$harvest[i]<-rpois(1, lakeFish.0[lakeFish.0$lakeID==anglers$lakeChoice[i],]$catch.param)
}

# replace 1 with t for time step once I make that loop
anglers$t=rep(1)

harvest<-rep(NA, length(lakeFish.0$lakeID))

# trackign nFish in lakes
for(i in 1:max(lakeFish.0$lakeID)){
  harvest[i]<-sum(anglers[anglers$lakeChoice==i,]$harvest)
}

lakeFish.app<-lakeFish.0

lakeFish.app$harvest<-harvest
lakeFish.app$nFish=lakeFish.app$nFish-lakeFish.app$harvest
# replace 1 with t for time loop
lakeFish.app$t<-rep(1)

lakeFish.app$catch.param<-ifelse(lakeFish.app$nFish==0, 0, lakeFish.app$catch.param)


