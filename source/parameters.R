# these are the parameters for the ABM

# Input parameters
parameters<-list()

# Lake landscape
nLakes          <-10 # 10 default lakes
edgeLength      <-50 # length of one side of landscape grid. default 50
nAnglers        <-50 # number of anglers to simulate. Default 50
nDays           <-100 # number of days (iterations) to simulate. Default 100
nFish0_min      <-1000 # minimum number of fish in a lake in the first time step
nDays           <-365 # days per annual loop
nYears          <-10  # number of years to simulate


# add parameters to list=========================================================
parameters[["nLakes"]]        <-nLakes
parameters[["edgeLength"]]    <-edgeLength
parameters[["nAnglers"]]      <-nAnglers
parameters[["nDays"]]         <-nDays
parameters[["nFish0_min"]]    <-nFish0_min
parameters[["nDays"]]         <-nDays
parameters[["nYears"]]        <-nYears