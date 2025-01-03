# ABM_dev
Agent based model of recreational anglers

ABM scripts are in the "source" folder and can be run from "run.model.template.R"

run.model.template.R sources function.sourcer.R, which sources each of the functions. So, if you make a new function, make sure to add it to function.sourcer.R

For long runs, it would be good to use a computing cluster; this thing is slow. 

This run of the model uses WDNR trend lakes. They are grouped into lake classes (based on Rypel et al 2019, Fisheries), and assigned VBGF parameters based on mean values for those classes. This is currently the only difference among lakes other than distance to anglers. In the future, differences among lakes could include natural mortality (from temperature) and more specific growth parameters. 

"parameters.R" is also in the source folder, and it's where you specify things like
the number of lakes to simulate, recruitment parameters, etc. 

How the model runs: 
1. Setting up the 'fishery' list. Each item on this list is made by its own function (function.name() or initialize.function.name() makes functionName object generally) which includes:
  * anglerCharacteristics--currently just location (lat long), but could include more in the future--preference categories, skill, avidity, travel cost, which could influence site choice or catch. Anglers are currently randomly distributed (uniform distribution in a bounding box) 
  * lakeCharacteristics--location, lake class (Rypel et al 2019, Fisheries), growth parameters, surface area, recruitment parameters. One row per lake
  * lakeDistance--pairwise distance between each lake and each angler, this is made from the lakeCharacteristics df
  * anglerDecisions (made by create.blank.angler.decisions())--empty data frame that will record angler decisions at each step of the simulation
  * fishPops (made by initialize.fish.pop.burnin())--a nested list that keeps track of fish populations in each simulated lake. This step creates the (mostly) blank nested list with some initial number of age-zero fish. This is currently set to give every lake the same population density (fish/hectare) defined by parameter N0. Rows are age classes and columns are years of the simulation. fishPops is updated throughout the 'year' in the simulation as fish are harvested
  * startPops (made by initialize.start.pop.burnin())--a copy of fishPops that keeps track of each year's starting population for each lake. 
  * selectivity (initialize.selectivity())--uses parameters to set the age at which fish are vulnerable to capture. 'selectivity' is a vector of length nAges (a parameter). This is currently a knife's edge selection "curve" (fish are fully capturable or impossible to capture) but could be replaced later. 
  * harvestAge, FmortAge, NmortAge--lists of matrices that track harvest-at-age, fishing mortality at age, and natural mortality at age throughout the simulation. Their 'initialize' scripts make blank lists. Multiple versions of initialize.NmortAge() exist in 'source/homeForRetiredScripts' Natural mortality is currently age-specific based on a study of walleye in Escanaba lake (Hansen et al 2011), and its function is set in parameter.R. This could later be changed to be affected by temperature. 
  * fishSizes (fish.size()) assigns age-specific length and weight based on lakes' VBGF parameters. The fishSizes object is a list of data frames

2. Burning in the fish populations--This runs an age structured population model xnBurnin, meant to get the fish populations to equilibrium. This runs the functions natural.mortality(), ageing(), recruitment(), and update.fishPops()

3. more initializing: initialize.start.pop(), initialize.fish.pop(), initialize.lake.status(), and initialize.annual.output() finish setting up the 'fishery' list and an annualOutput object that tracks annual changes during the simulation. annualOutput is good for visualization after the model run. 

4. The actual simulation has two nested loops. The big loop is years (set the number of years through nYears in parameters.R), the small loop is days. 
Each day: 
  * angler.decisions() --every angler chooses a lake. This is currently based on two parameters: betaTravel (a cost) and betaFish (benefit) based on distance to the lake and the number of fish in the lake, respectively. This script uses a random utility model to probablistically assign anglers based on maximum utility. At some point I wanted angler choice to instead be partly based on their catch history. 
  * fishing() --catch is currently deterministic based on N fish, effort (4 hours), selectivity/catchability, and a hyperstability parameter. I have been working on an analysis of angler catch distributions that can help with improving this function by adding angler skill coefficients, daily effects (i.e. changes in fishing conditions), and stochasticity.
After nDays, the simulation moves to the annual loop
* fishing.mortality() calculates age-specific mortality from fishing for each lake
* natural.mortality() applies age-specific natural mortality to each lake. M is negatively related to F
* ageing() moves the fish populations up a year, leaving a blank column for age 1
* recruitment() makes age 1 fish, which responds to stock density (based on population and surface area) and the alpha and beta parameters set in parameters.R. This does not currently have stochasticity. 
* annual.output() stores end-of-year stats in the annualOutput object
* update.lakes() updates the lakeStatus object
Then the year ends, the simulation moves back to the daily loop in year y+1


Scripts with different model runs:
- run.model.template.R (most up to date)
These may not run:
- run.model.R
- run.model.trend.lakes.R
- run.model.walleye.pop.charnov.R
run.model.walleye.pop.R