#Simulation Study - WITH COVAR
getwd()
setwd("C:/Users/danie/OneDrive/Statistiek master/Thesis/Thesis-LCA-danielle/Thesis-LCA-danielle/Sim_Covar_3parts")
start_time = Sys.time() #time, start counting

#load required packages
library(poLCA) #for data simulation
library(dplyr) #for data manipulation
library(confreq) #used for making bootstrap datasets 

#simulation parameters
nsim = 3 #increase to higher number of simulations when you have the time (nsim = 10 runt in 7.7 minuten)
nboot = 5 #FIXED, nboot determines the number of imputations per simulation iteration. 5 is sufficient.
populationsize = 5000 #for the data simulation

set.seed(123)
#execute respective scripts

source("Part1SimData.R")    #Simulate the data
source("Part2applyMILC.R")   #Apply MILC
source("Part3Results")       #Calculate results


#results
Average #the pooled average class sizes, average over simulations
trueclass #original simulated class sizes
bias = Average-trueclass

end_time = Sys.time()
end_time-start_time #elapsed time for script
save.image("Simulation_MILC.RData")
