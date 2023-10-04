# this holds the directories of all the functions for the ABM
# Set working directory, import packages, source functions
library(tidyverse)

setwd(paste0(base.directory, "/source/"))
source(paste0(getwd(), "/lake.location.R"))
source(paste0(getwd(), "/parameters.R"))
source(paste0(getwd(), "/angler.characteristics.R"))
source(paste0(getwd(),"/lake.distance.R"))
source(paste0(getwd(),"/lake.lambda.R"))
source(paste0(getwd(),"/create.blank.angler.decisions.R"))
source(paste0(getwd(),"/angler.decisions.R"))
source(paste0(getwd(),"/fishing.R"))