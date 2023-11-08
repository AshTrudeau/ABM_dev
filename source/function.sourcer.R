# this holds the directories of all the functions for the ABM
# Set working directory, import packages, source functions

setwd(paste0(base.directory, "/source/"))

# packages
library(tidyverse)
library(RColorBrewer)
library(cowplot)


# functions

source(paste0(getwd(), "/lake.location.R"))
source(paste0(getwd(), "/parameters.R"))
source(paste0(getwd(), "/angler.characteristics.R"))
source(paste0(getwd(),"/lake.distance.R"))
source(paste0(getwd(),"/lake.characteristics.R"))
source(paste0(getwd(),"/create.blank.angler.decisions.R"))
source(paste0(getwd(),"/angler.decisions.R"))
source(paste0(getwd(),"/fishing.R"))
# consider consolidating all these initialization scripts
source(paste0(getwd(), "/initialize.output.lakes.R"))
source(paste0(getwd(), "/initialize.fish.pop.R"))
source(paste0(getwd(), "/initialize.selectivity.R"))
source(paste0(getwd(), "/initialize.harvestAge.R"))
source(paste0(getwd(), "/initialize.FmortAge.R"))
source(paste0(getwd(), "/initialize.NmortAge.R"))
source(paste0(getwd(),"/output.script.R"))
source(paste0(getwd(),"/plotting.lake.status.R"))
source(paste0(getwd(), "/initialize.annual.output.R"))
source(paste0(getwd(), "/fishing.mortality.R"))
source(paste0(getwd(), "/natural.mortality.R"))
source(paste0(getwd(), "/ageing.R"))
source(paste0(getwd(), "/recruitment.R"))
source(paste0(getwd(), "/annual.output.R"))
source(paste0(getwd(),"/update.lakes.R"))
