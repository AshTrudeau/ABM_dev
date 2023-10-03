# these are the parameters for the ABM

# Input parameters
parameters<-list()

# Lake landscape
nLakes          <-10 # 10 default lakes
edgeLength      <-50 # length of one side of landscape grid. default 50
nAnglers        <-50 # number of anglers to simulate. Default 50
nDays           <-100 # number of days (iterations) to simulate. Default 100


# add parameters to list=========================================================
parameters[["nLakes"]]        <-nLakes
parameters[["edgeLength"]]    <-edgeLength
parameters[["nAnglers"]]      <-nAnglers
parameters[["nDays"]]         <-nDays