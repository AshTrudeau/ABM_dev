# these are the parameters for the ABM

# Input parameters
parameters<-list()

# Lake landscape
nLakes          <-1 # 10 default lakes
edgeLength      <-50 # length of one side of landscape grid. default 50
nAnglers        <-10 # number of anglers to simulate. Default 50
nDays           <-365 # number of days (iterations) per year to simulate. Default 365
nYears          <-10 # Number of years (big loops) to simulate, default 10
nFish0_min      <-5000 # minimum number of fish in a lake in the first time step
beta            <-0.5 # hyperstability parameter
q               <-0.1 # catchability coefficient
# clunkily set bounding box for angler placement
maxLat         <-46.221210
maxLong        <- -88.971443
minLat         <- 45.923449
minLong        <- -89.418459
# regional values from Tsehaye et al 2016
alpha          <-2.768
beta           <-0.049
sigma          <-1.964
# starting number of fish
N0             <-5000
nAges          <-15
# natural mort
allAges       <-c(0:nAges)
M              <-0.7*exp(-0.114*allAges)# age specific natural mortality. (Escanaba, Hansen et al 2011) (later tie to temperature and fishing mortality)
                        # using log relationship listed in Tsehaye paper. (used only for ages up to 5, but using it now for all ages for simplicity)
                        # not yet reducing natural mortality as fishing mortality increases; add that later
ageVulnerable  <-3      # age vulnerable to fishing mortality (assuming knife edge)

# add parameters to list=========================================================
parameters[["nLakes"]]        <-nLakes
parameters[["edgeLength"]]    <-edgeLength
parameters[["nAnglers"]]      <-nAnglers
parameters[["nDays"]]         <-nDays
parameters[["nFish0_min"]]    <-nFish0_min
parameters[["nDays"]]         <-nDays
parameters[["nYears"]]        <-nYears
parameters[["beta"]]          <-beta
parameters[["q"]]             <-q
parameters[["M"]]             <-M
parameters[["maxLat"]]        <-maxLat
parameters[["maxLong"]]       <-maxLong
parameters[["minLat"]]        <-minLat
parameters[["minLong"]]       <-minLong
parameters[["alpha"]]         <-alpha
parameters[["beta"]]          <-beta
parameters[["sigma"]]         <-sigma
parameters[["N0"]]            <-N0
parameters[["nAges"]]         <-nAges
parameters[["allAges"]]      <-allAges
parameters[["ageVulnerable"]] <-ageVulnerable