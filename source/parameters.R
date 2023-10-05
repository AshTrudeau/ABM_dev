# these are the parameters for the ABM

# Input parameters
parameters<-list()

# Lake landscape
nLakes          <-10 # 10 default lakes
edgeLength      <-50 # length of one side of landscape grid. default 50
nAnglers        <-50 # number of anglers to simulate. Default 50
nDays           <-365 # number of days (iterations) per year to simulate. Default 365
nYears          <-10 # Number of years (big loops) to simulate, default 10
nFish0_min      <-1000 # minimum number of fish in a lake in the first time step
nDays           <-365 # days per annual loop
nYears          <-10  # number of years to simulate
beta            <-0.5 # hyperstability parameter
q               <-0.001 # catchability coefficient


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