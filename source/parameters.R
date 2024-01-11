# these are the parameters for the ABM

# Input parameters
parameters<-list()

# Lake landscape
nLakes          <-25 # 10 default lakes
#edgeLength      <-50 # length of one side of landscape grid. default 50
nAnglers        <-50 # number of anglers to simulate. Default 50
nDays           <-50 # number of days (iterations) per year to simulate. Default 365
nYears          <-10 # Number of years (big loops) to simulate, default 10
nBurnIn         <-50 # years of unexploited fish population dynamics before the simulation. 50 was about
# right with a starting fish population of 5000
#nFish0_min      <-5000 # minimum number of fish in a lake in the first time step
beta            <-0.5 # hyperstability parameter
q               <-0.01 # catchability coefficient
# clunkily set bounding box for angler placement
maxLat         <- 46.99
maxLong        <- -86.7
minLat         <- 42.5
minLong        <- -92.98
# regional values of recruitment parameters from Tsehaye et al 2016
recAlpha          <- 9.88
recBeta           <- 0.29
# reducing this from 2.15 because the stochasticity is nuts; huge jumps in recruitment
recSigma          <- 1
# starting number of fish per hectare
N0             <-4
nAges          <-15
# natural mort
allAges       <-c(0:nAges)
M              <-0.7*exp(-0.114*allAges)# age specific natural mortality. (Escanaba, Hansen et al 2011) (later tie to temperature and fishing mortality)
                        # using log relationship listed in Tsehaye paper. (used only for ages up to 5, but using it now for all ages for simplicity)
                        # not yet reducing natural mortality as fishing mortality increases; add that later
ageVulnerable  <-2      # age vulnerable to fishing mortality (assuming knife edge)
ageMature      <-2

# quality size fish in cm
qualitySize     <-38.1

# behavior model
# travel cost param, utility per km
betaTravel=-0.05
# catch utility param, utility per fish in lake
betaFish=0.0001


# add parameters to list=========================================================
parameters[["nLakes"]]        <-nLakes
#parameters[["edgeLength"]]    <-edgeLength
parameters[["nAnglers"]]      <-nAnglers
parameters[["nDays"]]         <-nDays
#parameters[["nFish0_min"]]    <-nFish0_min
parameters[["nDays"]]         <-nDays
parameters[["nYears"]]        <-nYears
parameters[["nBurnIn"]]       <-nBurnIn
parameters[["beta"]]          <-beta
parameters[["q"]]             <-q
parameters[["M"]]             <-M
parameters[["maxLat"]]        <-maxLat
parameters[["maxLong"]]       <-maxLong
parameters[["minLat"]]        <-minLat
parameters[["minLong"]]       <-minLong
parameters[["recAlpha"]]         <-recAlpha
parameters[["recBeta"]]          <-recBeta
parameters[["recSigma"]]         <-recSigma
parameters[["N0"]]            <-N0
parameters[["nAges"]]         <-nAges
parameters[["allAges"]]      <-allAges
parameters[["ageVulnerable"]] <-ageVulnerable
parameters[["ageMature"]]     <-ageMature
parameters[["qualitySize"]]   <-qualitySize
parameters[["betaTravel"]]    <-betaTravel
parameters[["betaFish"]]      <-betaFish