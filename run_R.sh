#!/bin/bash

# untar your R installation. Make sure you are using the right version!
tar -xzf R413.tar.gz
# (optional) if you have a set of packages (created in Part 1), untar them also
tar -xzf packages.tar.gz

# make sure the script will use your R installation, 
# and the working directory as its home location
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages

# run your script
Rscript run.model.R

# mkidr my_output
# mv annual.output.csv lake.status.csv my_output/
# tar -czf my_job.output.tar.gz my_output/
# to give executable permissions, run `chmod +x run_R.sh` in submit directory# create output
