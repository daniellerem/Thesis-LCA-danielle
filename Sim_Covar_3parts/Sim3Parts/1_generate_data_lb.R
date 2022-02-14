library(poLCA) 
library(confreq) 
library(dplyr) 

options(scipen = 999)

nsim   = 20 
nsize  = 5000 
nboot  = 5
nconds = 8

set.seed(123)

source("1a_specify_simconditions.R")                                            # load a function that generates the sim. conditions
source("1b_simulate_dataset.R")
source("1c_generate_bootstraps.R")

sim_conds = simconds()                                                          # run the sim. conditions function
simbootdat = vector("list", nconds)


for(i in 1:nconds){                                                             # loop over conditions
  cat(i)
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    sim_data = simdata(nsize      = nsize,                                      # generate a simulated dataset
                       sel_error  = sim_conds[[i]][[1]],
                       meas_error = sim_conds[[i]][[2]])
    
    
    simbootdat[[i]][[j]] = bootdata(data  = sim_data,                           # bootstraps of the simulated dataset
                                 nboot = nboot)
    
  }
}
save(simbootdat, file = "simbootdat.RData")

